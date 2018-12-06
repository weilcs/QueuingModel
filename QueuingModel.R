set.seed(1)
N = 100
customer = seq(N)
interval_time = rexp(N, 1)
arrive_time = c()
arrive_time[1] = 0
for (i in 1:(N-1)){
  arrive_time[i+1] = arrive_time[i] + interval_time[i+1]
}

service_time = rexp(N, 1)
#service_time = rnorm(N, mean = 1, sd = 0.1)
service_start_time = c()
service_start_time[1] = 0
for (i in 2:N){
  service_start_time[i] = service_start_time[i-1]+service_time[i-1]
  if(service_start_time[i]<arrive_time[i]){
    service_start_time[i] = arrive_time[i]
  }
}

service_end_time = c()
for (i in 1:N){
  service_end_time[i] = service_start_time[i]+service_time[i]
}

idle = c()
idle[1] = 0
for (i in 2:N){
  idle[i] = arrive_time[i] - service_end_time[i-1]
  if (idle[i]<0){idle[i] = 0}
}


wait = c()
count_wait = 0
for (i in 1:N){
  wait[i] = service_start_time[i] - arrive_time[i]
  if (wait[i]!=0){count_wait = count_wait+1}
}

time_spent = wait + service_time



my_table = data.frame(customer, interval_time, arrive_time, service_start_time, 
          service_time, service_end_time, time_spent, wait, idle)

#Total waiting time in queue for all customers
total_wait = sum(wait)

#Average waiting time per customer
mean_wait = mean(wait)

#Average waiting time for customers that have to wait
average_wait = total_wait/count_wait

#The probability for a customer to wait in the queue
p_wait = count_wait/N

#Total idle time
total_idle = sum(idle)

#The proportion of idle time of the server
p_idle = total_idle/(service_end_time[N]-service_start_time[1])

#Total service time
total_service = sum(service_time)

#Average service time
#Expected service time should be 1 because it's normal distribution
mean_service = total_service/N

#Average time between arrivals
#Expected average time between arrivals should be 1 because of exponential distribution
average_inter_time = (arrive_time[N] - arrive_time[1])/(N-1)

#Average time spent by a customer in the system
mean_time_spent = sum(time_spent)/N

L = list(total_wait,mean_wait,average_wait,p_wait,total_idle,p_idle,mean_service,average_inter_time,mean_time_spent)

a = rbind.data.frame(L)
