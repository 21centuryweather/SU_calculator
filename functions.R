queue_param <-fread("queue_limits.csv")

# Node and queue definitions
node_queues <- list(
  "Normal nodes (Cascade Lake)" = queue_param[node_type == "cascade_lake", queue],
  "ARE (Broadwell)" = queue_param[node_type == "broadwell", queue],
  "GPU" = queue_param[node_type == "gpu", queue]
)

is_valid_cpu <- function(input_cpu, this_queue) {
  valid <- TRUE
  if (this_queue %in% c("normal", "express", "hugemen", "megamen")) {
    if (input_cpu > 48 & input_cpu %% 48 != 0) {
      valid <-  FALSE
    }
  } else if (this_queue == "copyq") { 
    if (input_cpu != 1) {
      valid <-  FALSE
    }
  } else if (this_queue %in% c("normalbw", "expressbw")) {
    if (input_cpu > 28 & input_cpu %% 28 != 0) {
      valid <-  FALSE
    }
  } else if (this_queue == "hugemembw") { 
    if (!(input_cpu %in% c(7, 14, 21, 28)) & input_cpu %% 28 != 0) {
      valid <-  FALSE
    }
  } else if (this_queue == "megamembw") { 
    if (!(input_cpu %in% c(32, 64))) {
      valid <-  FALSE
    }
  } else if (this_queue == "dgxa100") { 
    if (input_cpu %% 16 != 0) {
      valid <-  FALSE
    }
  } else if (this_queue == "gpuhopper") { 
    if (!(input_cpu %in% c(12, 24, 36, 48)) & input_cpu %% 48 != 0) {
      valid <-  FALSE
    }
  } else if (this_queue == "gpuvolta") { 
    if (input_cpu %% 12 != 0) {
      valid <-  FALSE
    }
  }
  
  return(valid)
}

cpu_message <- function(input_cpu, this_queue, max_cpus) {
  
  if(input_cpu > max_cpus) {
    return(tags$span(paste("Max CPU/GPUs for this queue:", max_cpus),
                     class = "limit-exceeded"))
  }
  
  
  valid_ncpu <- is_valid_cpu(input_cpu, this_queue)
  
  if (!valid_ncpu) {
    return(tags$span(paste("Invalid CPU/GPUs for this queue:", queue_param[queue == this_queue, cpu_limits[1]]),
                     class = "limit-exceeded"))      
  }
  
  return(tags$span(paste("Max CPU/GPUs for this queue:", max_cpus),
                   class = "limit-ok"))
}

get_walltime_limit <- function(this_queue, this_cpus) {
  
  queue_param[queue == this_queue & max_cpus >= this_cpus] |> 
    _[max_cpus == min(max_cpus), max_walltime]
  
}

get_memory_limit <- function(this_queue) {
  
  queue_param[queue == this_queue] |> 
    _[, unique(max_memory)]
  
}

calculate_memory_blocks <- function(this_queue) {
  
  queue_mem_per_node <- get_memory_limit(this_queue)
  queue_cpu_per_node <-   queue_param[queue == this_queue] |> 
    _[, unique(cpu_per_node)]
  
  queue_cpu_per_node/queue_mem_per_node
}

calculate_SU <- function(this_queue, cpus, memory, walltime) {
  
  queue_cost = queue_param[this_queue == queue, unique(cost)]
  
  # Memory is charged in blocks depending on the queue
  # memory = (ncpus_per_node/mem_per_node)*(mem_request/ncpus_request)
  memory =  calculate_memory_blocks(this_queue) * memory/cpus
  
  round(queue_cost * cpus * max(1, memory) * walltime )
  
}