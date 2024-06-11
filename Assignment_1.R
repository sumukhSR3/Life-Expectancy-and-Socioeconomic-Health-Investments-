library(readr)
simdata <- read_csv("Desktop/simdata.csv")
View(simdata)
str(simdata)
library(ggplot2)
# Plotting on-hand inventory levels
ggplot(simdata, aes(x = Day)) +
  geom_line(aes(y = `On hand Inventory...4`, color = "Retailer Inventory")) +
  geom_line(aes(y = `On hand Inventory...11`, color = "Vendor Inventory")) +
  labs(title = "On-Hand Inventory at Retailer and Vendor",
       x = "Day", y = "Inventory Level") +
  scale_color_manual(values = c("Retailer Inventory" = "blue", "Vendor Inventory" = "red")) +
  theme_minimal()
# Visualizing Demand Status
ggplot(simdata, aes(x = `Demand Status`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Demand Status Frequency",
       x = "Demand Status", y = "Count") +
  theme_minimal()

# Scatter plot of Total Demand vs. Quantity Delivered
ggplot(simdata, aes(x = `Total Demand`, y = `Quantity Delivered`)) +
  geom_point(aes(color = `Demand Status`), alpha = 0.7) +
  labs(title = "Total Demand vs. Quantity Delivered",
       x = "Total Demand", y = "Quantity Delivered") +
  scale_color_manual(values = c("Filled" = "green", "Lost" = "red")) +
  theme_minimal()
