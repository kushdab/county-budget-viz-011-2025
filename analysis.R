## county-budget-viz-011-2025
## Analysis of Kenyan County Budget Allocations and Spending

library(tidyverse)
library(scales)
library(gridExtra)

# --- 1. Data Generation (Simulating 47 Counties) ---
set.seed(2025)
counties <- c(
  "Mombasa", "Kwale", "Kilifi", "Tana River", "Lamu", "Taita/Taveta", 
  "Garissa", "Wajir", "Mandera", "Marsabit", "Isiolo", "Meru", 
  "Tharaka-Nithi", "Embu", "Kitui", "Machakos", "Makueni", "Nyandarua", 
  "Nyeri", "Kirinyaga", "Murang'a", "Kiambu", "Turkana", "West Pokot", 
  "Samburu", "Trans Nzoia", "Uasin Gishu", "Elgeyo/Marakwet", "Nandi", 
  "Baringo", "Laikipia", "Nakuru", "Narok", "Kajiado", "Kericho", 
  "Bomet", "Kakamega", "Vihiga", "Bungoma", "Busia", "Siaya", 
  "Kisumu", "Homa Bay", "Migori", "Kisii", "Nyamira", "Nairobi City"
)

sectors <- c("Health", "Infrastructure", "Education", "Agriculture", "Water & Environment", "Governance")

# Create a long-form dataframe
budget_data <- expand.grid(County = counties, Sector = sectors) %>%
  mutate(
    Allocated_KES_M = runif(n(), 200, 3500),
    Spending_Efficiency = runif(n(), 0.55, 0.98),
    Spent_KES_M = Allocated_KES_M * Spending_Efficiency
  )

# --- 2. Data Aggregation ---
county_summary <- budget_data %>%
  group_by(County) %>%
  summarize(
    Total_Allocated = sum(Allocated_KES_M),
    Total_Spent = sum(Spent_KES_M),
    Avg_Efficiency = mean(Spending_Efficiency) * 100
  ) %>%
  arrange(desc(Total_Allocated))

sector_summary <- budget_data %>%
  group_by(Sector) %>%
  summarize(
    Total_Allocated = sum(Allocated_KES_M),
    Total_Spent = sum(Spent_KES_M),
    Absorption_Rate = (Total_Spent / Total_Allocated) * 100
  )

# --- 3. Visualization: Budget Size vs. Spending Efficiency ---
p1 <- ggplot(county_summary, aes(x = Total_Allocated, y = Avg_Efficiency)) +
  geom_point(aes(size = Total_Spent, color = Avg_Efficiency), alpha = 0.7) +
  geom_text(aes(label = ifelse(Total_Allocated > quantile(Total_Allocated, 0.9), County, "")), 
            vjust = -1, size = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "County Budget Size vs. Spending Efficiency",
    subtitle = "Highlighting top spending counties (Values in Millions KES)",
    x = "Total Allocated Budget",
    y = "Average Absorption Rate (%)",
    size = "Total Spent"
  ) +
  theme_minimal()

# --- 4. Visualization: Top 10 Counties Sector Breakdown ---
top_10_counties <- head(county_summary$County, 10)
p2 <- budget_data %>%
  filter(County %in% top_10_counties) %>%
  ggplot(aes(x = reorder(County, Allocated_KES_M), y = Allocated_KES_M, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Sectoral Allocation for Top 10 Highest Funded Counties",
    x = "County",
    y = "Allocation (Millions KES)"
  ) +
  theme_light()

# --- 5. Visualization: Sectoral Absorption Rates ---
p3 <- ggplot(sector_summary, aes(x = reorder(Sector, Absorption_Rate), y = Absorption_Rate, fill = Sector)) +
  geom_col() +
  geom_hline(yintercept = mean(sector_summary$Absorption_Rate), linetype = "dashed", color = "red") +
  annotate("text", x = 1.5, y = mean(sector_summary$Absorption_Rate) + 2, label = "National Average", color = "red") +
  labs(
    title = "Budget Absorption Rate by Sector",
    x = "Sector",
    y = "Absorption Rate (%)"
  ) +
  theme_minimal() + 
  theme(legend.position = "none")

# --- 6. Executive Summary Output ---
cat("\n--- County Budget Analysis Report 2025 ---\n")
cat("Total National Allocation Explored: ", sum(county_summary$Total_Allocated), " Million KES\n")
cat("Highest Budget County: ", county_summary$County[1], "\n")
cat("Sector with Highest Absorption: ", sector_summary$Sector[which.max(sector_summary$Absorption_Rate)], "\n")

# Print Plots
print(p1)
print(p2)
print(p3)

# End of script