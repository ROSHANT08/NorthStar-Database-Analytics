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
drivers <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/drivers.csv")
hubs <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/hubs.csv")
incidents <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/incidents.csv")
vehicles <- read_csv("C:/Users/USER/Documents/UWL/Database and analytics/Assignment/northstar_dataset/vehicles.csv")

head(customers)
head(orders)
head(deliveries)
head(complaints)
head(drivers)
head(hubs)
head(incidents)
head(vehicles)

#query 1
result1 <- sqldf("
SELECT delivery_status, COUNT(*) AS total
FROM deliveries
GROUP BY delivery_status
")
print(result1)

#query 2
result2 <- sqldf("
    SELECT complaint_type,
           COUNT(*) as total_complaints,
           AVG(compensation_amount) as avg_compensation,
           AVG(resolution_days) as avg_resolution_days
    FROM complaints
    GROUP BY complaint_type
    ORDER BY total_complaints DESC
")
print(result2)

#query 3
result3 <- sqldf("
    SELECT h.zone, COUNT(*) as failed_count
    FROM deliveries d
    JOIN hubs h ON d.hub_id = h.hub_id
    WHERE d.delivery_status = 'Failed'
    GROUP BY h.zone
    ORDER BY failed_count DESC
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
    SELECT d.driver_id,
           COUNT(dl.delivery_id) as total_deliveries,
           SUM(CASE WHEN dl.delivery_status = 'Failed' THEN 1 ELSE 0 END) as failed_deliveries,
           ROUND(AVG(dl.customer_rating_post_delivery), 2) as avg_rating
    FROM drivers d
    JOIN deliveries dl ON d.driver_id = dl.driver_id
    GROUP BY d.driver_id
    HAVING total_deliveries > 5
    ORDER BY failed_deliveries DESC
    LIMIT 10
")
print(result6)


#query 7
result7 <- sqldf("
SELECT
delivery_status,
AVG(fuel_or_charge_cost) AS avg_cost
FROM deliveries
GROUP BY delivery_status
")

print(result7)

#query 8
result8 <- sqldf("
    SELECT c.home_zone,
           COUNT(cp.complaint_id) as total_complaints,
           AVG(c.loyalty_score) as avg_loyalty
    FROM customers c
    LEFT JOIN complaints cp ON c.customer_id = cp.customer_id
    GROUP BY c.home_zone
    ORDER BY total_complaints DESC
")
print(result8)

#query 9
result9 <- sqldf("
    SELECT o.priority_level,
           COUNT(o.order_id) as total_orders,
           SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) as failed_orders,
           ROUND(100.0 * SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) / COUNT(o.order_id), 2) as failure_rate
    FROM orders o
    LEFT JOIN deliveries d ON o.order_id = d.order_id
    WHERE o.priority_level IN ('High', 'Critical')
    GROUP BY o.priority_level
")
print(result9)

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
ggplot(result2, 
       aes(x = reorder(complaint_type, -total_complaints),
           y = total_complaints,
           fill = complaint_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Complaints by Type",
       x = "Complaint Type",
       y = "Number of Complaints") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#visualization 3
ggplot(complaints,
       aes(x = severity)) +
  geom_bar(fill = "red") +
  theme_minimal() +
  labs(
    title = "Complaint Severity Distribution",
    x = "Severity",
    y = "Count"
  )

#visualization 4
deliveries_with_zone <- merge(deliveries, hubs, by = "hub_id")

failed_zone <- as.data.frame(
  table(deliveries_with_zone$zone[deliveries_with_zone$delivery_status == "Failed"])
)

names(failed_zone) <- c("zone", "failed_count")

failed_zone <- failed_zone[order(-failed_zone$failed_count), ]

ggplot(failed_zone,
       aes(x = reorder(zone, -failed_count),
           y = failed_count,
           fill = zone)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = failed_count), vjust = -0.5) +
  labs(title = "Failed Deliveries by Zone",
       x = "Zone",
       y = "Number of Failures") +
  theme_minimal() +
  theme(legend.position = "none")

#visualization 5
driver_workload <- as.data.frame(table(deliveries$driver_id))
names(driver_workload) <- c("driver_id", "delivery_count")
top_drivers <- head(driver_workload[order(-driver_workload$delivery_count), ], 10)

ggplot(top_drivers, aes(x = reorder(driver_id, -delivery_count), y = delivery_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = delivery_count), vjust = -0.5) +
  labs(title = "Top 10 Busiest Drivers", x = "Driver ID", y = "Number of Deliveries") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





