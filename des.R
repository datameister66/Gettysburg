library(simmer)
library(magrittr)
library(simmer.plot)

# Kendall's M/M/c/k
# Arrival/service time distribution/number of servers/max allowed in the system

set.seed(1966)
(env <- simmer("Job Shop"))
RUNNING <- function() floor(runif(1, min = 1, max = 4))
RETOOL <- function() runif(1, 1, 1)
RESET <- function() runif(1, 1, 1)
AWAY <- function() runif(1, 1, 1)
CHECK_WORN <- function() runif(1) < 0.25
NEW_JOB <- function() runif(1, 1, 1)
NEW_TASK <- function() runif(1, 1, 1)

# Ready -> runs -> retool? -> reset -> Ready
# away friction pulling operatives
job <- trajectory() %>%
   seize("machine") %>% # resource
   timeout(RUNNING) %>% # runs a job
   branch( # a branch is a decision i.e. a Fork in the trajectory path
     CHECK_WORN, continue = TRUE,
     trajectory() %>%
       seize("operative") %>% # this is the operator resource for this branch
       timeout(RETOOL) %>% # The action of the branch / the time in the system
       release("operative") # returns the trajectory object i.e. this branch
     ) %>%
   seize("operative") %>% # operator needs to reset regardless of retool
   timeout(RESET) %>% # running the machine reset
   release("operative") %>% #operator done
   release("machine") # machine is done

task <- trajectory() %>% # this is personal tasks "mandatory training" etc.
   seize("operative") %>%
   timeout(AWAY) %>%
   release("operative")

# append 10 machines and 5 operators to the environment
env %>%
  add_resource("machine", 4) %>%
  add_resource("operative", 2) %>%
  # generate jobs and tasks from above
  add_generator("job", job, NEW_JOB) %>% # name prefix, trajectory name, distribution
  add_generator("task", task, NEW_TASK) %>%
  run(until=50)

aggregate(cbind(server, queue) ~ resource, get_mon_resources(env), mean)
get_mon_resources(env)

resources <- get_mon_resources(env)
arrivals <- get_mon_arrivals(env)

plot(resources, metric="usage", "machine", items = "server", steps = TRUE)
plot(resources, metric = "usage", "machine")
plot(resources, metric = "usage", "operative")
plot(resources, metric = "usage", "operative", items ='server', steps = TRUE)
plot(resources, metric="utilization", c("machine", "operative"))
plot(arrivals, metric="waiting_time")
plot(arrivals, metric="activity_time")
plot(arrivals, metric="flow_time")

plot(job)
plot(task)

# Server - a resource seized and released; It is passive
# Active - generator, arrival, manager
# Queue - priority queue of certain size
#Trajectory An interlinkage of activities constituting a recipe for arrivals attached to it

# Arrival properties; see documentation

# resource interaction is SEIZE and RELEASE

# --------------------
patient <- trajectory() %>%
  log_("arriving...") %>%
  seize(
    "doctor", 1, continue = c(TRUE, FALSE),
    post.seize = trajectory("accepted patient") %>% # a successful seize
      log_("doctor seized"),
    reject = trajectory("rejected patient") %>% # an unsuccessful seize
      log_("rejected!") %>%
      seize("nurse", 1) %>%
      log_("nurse seized") %>%
      timeout(2) %>%
      release("nurse", 1) %>%
      log_("nurse released")
    ) %>%
  timeout(5) %>%
  release("doctor", 1) %>%
  log_("doctor released")

simmer() %>%
  add_resource("doctor", capacity = 1, queue_size = 0) %>%
  add_resource("nurse", capacity = 2, queue_size = 0) %>%
  add_generator("patient", patient, at(0, 1)) %>%
  run()

# generators (active and deactivate)

# Branches
  # can be equivalent to if/else or
  # clone()

# Loops

# Batching

# Reneging
  # leave()

now(env)
peek(env)
peek(env, steps = 10)
get_mon_arrivals(env)
get_mon_resources(env)
get_mon_attributes(env)


# M/M/1 -------------------------------------------------------------------

set.seed(1234)

lambda <- 2
mu <- 4
rho <- lambda/mu

mm1.traj <- trajectory() %>%
seize("mm1.resource", amount=1) %>%
timeout(function() rexp(1, mu)) %>%
release("mm1.resource", amount=1)

mm1.env <- simmer() %>%
  add_resource("mm1.resource", capacity=1, queue_size=Inf) %>%
  add_generator("arrival", mm1.traj, function() rexp(1, lambda)) %>%
  run(until=2000)
resources <- get_mon_resources(mm1.env)
plot(resources, metric = "usage")
plot(resources, metric = "utilization")

arrivals <- get_mon_arrivals(mm1.env)
plot(arrivals, metric = "waiting_time")
summary(arrivals)

plot(mm1.traj)
# metrics = resources(usage, utilization)
# arrivals(activity_time, waiting_time, flow_time)


# plots -------------------------------------------------------------------

t0 <- trajectory("my trajectory") %>%
  ## add an intake activity
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 10)) %>%
  release("nurse", 1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 10)) %>%
  release("administration", 1)

env <- simmer() %>%
  add_resource("nurse", 5) %>%
  add_resource("doctor", 2) %>%
  add_resource("administration", 1) %>%
  add_generator("patient", t0, function() runif(1, 5, 15)) %>%
  run(until = 500)

resources <- get_mon_resources(env)
arrivals <- get_mon_arrivals(env)

plot(resources, metric="usage", "doctor", items = "server", steps = TRUE)
plot(resources, metric="utilization", c("nurse", "doctor", "administration"))
plot(arrivals, metric="waiting_time")
plot(arrivals, metric="activity_time")
plot(arrivals, metric="flow_time")

plot(t0)
