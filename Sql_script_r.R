install.packages("sqldf")
install.packages("readr")
install.packages("dplyr")

library(sqldf)
library(readr)
library(dplyr)

customers <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/customers.csv")
orders <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/orders.csv")
deliveries <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/deliveries.csv")
complaints <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/complaints.csv")


head(customers)
head(orders)
head(deliveries)

#query 1
result1 <- sqldf("
SELECT delivery_status, COUNT(*) AS total
FROM deliveries
GROUP BY delivery_status
")

print(result1)

#query 2
result2 <- sqldf("
SELECT complaint_type, COUNT(*) AS total_complaints
FROM complaints
GROUP BY complaint_type
ORDER BY total_complaints DESC
")

print(result2)


deliveries$dispatch_time <- as.POSIXct(deliveries$dispatch_time)

deliveries$delivery_completed_at <- as.POSIXct(
  deliveries$delivery_completed_at
)

deliveries$delivery_duration_minutes <- as.numeric(
  difftime(
    deliveries$delivery_completed_at,
    deliveries$dispatch_time,
    units = "mins"
  )
)

head(deliveries$delivery_duration_minutes)

#query 3
result3 <- sqldf("
SELECT delivery_status,
AVG(delivery_duration_minutes) AS avg_delivery_time
FROM deliveries
GROUP BY delivery_status
")

print(result3)

#query 4
result4 <- sqldf("
SELECT
d.delivery_status,
COUNT(c.complaint_id) AS total_complaints
FROM deliveries d
LEFT JOIN complaints c
ON d.order_id = c.order_id
GROUP BY d.delivery_status
")

print(result4)

#query 5
result5 <- sqldf("
SELECT
driver_id,
AVG(manual_route_override_count) AS avg_override
FROM deliveries
GROUP BY driver_id
ORDER BY avg_override DESC
LIMIT 10
")

print(result5)

#query 6
result6 <- sqldf("
SELECT
delivery_status,
AVG(fuel_or_charge_cost) AS avg_cost
FROM deliveries
GROUP BY delivery_status
")

print(result6)



install.packages("ggplot2")

#visualization 1 
ggplot(deliveries,
       aes(x = delivery_status)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Delivery Status Distribution",
    x = "Delivery Status",
    y = "Count"
  )

#visualization 2
ggplot(complaints,
       aes(x = severity)) +
  geom_bar(fill = "red") +
  theme_minimal() +
  labs(
    title = "Complaint Severity Distribution",
    x = "Severity",
    y = "Count"
  )



