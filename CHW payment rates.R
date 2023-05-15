# This code reads input data from a CSV file, loops through each row (representing a state), calculates the minimum payment per visit and per month per patient using the input variables, and estimates the mean and 95% confidence intervals of the results based on additional mean and standard deviation information in the input data. The output is saved to a CSV file.


setwd("~/Downloads")
library(dplyr)
library(officer)
library(flextable)
library(ggplot2)
library(maps)

# Load input data
input_data <- read.csv("input.csv")  

# Initialize output data frame
output_data <- data.frame(state = character(),
                           min_payment_per_visit_mean = numeric(),
                          min_payment_per_visit_lower = numeric(),
                          min_payment_per_visit_upper = numeric(),
                          min_payment_per_month_mean = numeric(),
                          min_payment_per_month_lower = numeric(),
                          min_payment_per_month_upper = numeric(),
                          stringsAsFactors = FALSE)

# Loop through each state in input data and calculate payments
for (i in 1:nrow(input_data)) {
  state <- input_data[i, "state"]
  chw_wage_mean <- input_data[i, "chw_wage"]
  chw_wage_sd <- input_data[i, "chw_wage_sd"]
  chw_wage_n <- input_data[i, "chw_wage_n"]
  chw_overhead_mean <- input_data[i, "chw_overhead"]
  chw_overhead_sd <- input_data[i, "chw_overhead_sd"]
  chw_overhead_n <- input_data[i, "chw_overhead_n"]
  supervisor_wage_mean <- input_data[i, "supervisor_wage"]
  supervisor_wage_sd <- input_data[i, "supervisor_wage_sd"]
  supervisor_wage_n <- input_data[i, "supervisor_wage_n"]
  supervisor_overhead_mean <- input_data[i, "supervisor_overhead"]
  supervisor_overhead_sd <- input_data[i, "supervisor_overhead_sd"]
  supervisor_overhead_n <- input_data[i, "supervisor_overhead_n"]
  visit_length_mean <- input_data[i, "visit_length"]
  visit_length_sd <- input_data[i, "visit_length_sd"]
  visit_length_n <- input_data[i, "visit_length_n"]
  transport_time_per_visit_mean <- input_data[i, "transport_time_per_visit"]
  transport_time_per_visit_sd <- input_data[i, "transport_time_per_visit_sd"]
  transport_time_per_visit_n <- input_data[i, "transport_time_per_visit_n"]
  cohort_size_mean <- input_data[i, "cohort_size"]
  cohort_size_sd <- input_data[i, "cohort_size_sd"]
  cohort_size_n <- input_data[i, "cohort_size_n"]
  phone_cost_mean <- input_data[i, "phone_cost"]
  phone_cost_sd <- input_data[i, "phone_cost_sd"]
  phone_cost_n <- input_data[i, "phone_cost_n"]
  laptop_cost_mean <- input_data[i, "laptop_cost"]
  laptop_cost_sd <- input_data[i, "laptop_cost_sd"]
  laptop_cost_n <- input_data[i, "laptop_cost_n"]
  itcost_mean <- input_data[i, "it_cost"]
  itcost_sd <- input_data[i, "it_cost_sd"]
  itcost_n <- input_data[i, "it_cost_n"]
  ehr_cost_mean <- input_data[i, "ehr_cost"]
  ehr_cost_sd <- input_data[i, "ehr_cost_sd"]
  ehr_cost_n <- input_data[i, "ehr_cost_n"]
  depr_cost_mean <- input_data[i, "depr_cost"]
  depr_cost_sd <- input_data[i, "depr_cost_sd"]
  depr_cost_n <- input_data[i, "depr_cost_n"]
  space_cost_mean <- input_data[i, "space_cost"]
  space_cost_sd <- input_data[i, "space_cost_sd"]
  space_cost_n <- input_data[i, "space_cost_n"]

  
  # Sample from input variable distributions
  chw_wage <- rnorm(10000, chw_wage_mean, chw_wage_sd/sqrt(chw_wage_n))
  chw_overhead <- rnorm(10000, chw_overhead_mean, chw_overhead_sd/sqrt(chw_overhead_n))
  supervisor_wage <- rnorm(10000, supervisor_wage_mean, supervisor_wage_sd/sqrt(supervisor_wage_n))
  supervisor_overhead <- rnorm(10000, supervisor_overhead_mean, supervisor_overhead_sd/sqrt(supervisor_overhead_n))
  visit_length <- rnorm(10000, visit_length_mean, visit_length_sd/sqrt(visit_length_n))
  visit_length[visit_length<=0] = mean(visit_length)
  transport_time_per_visit <- rnorm(10000, transport_time_per_visit_mean, transport_time_per_visit_sd/sqrt(transport_time_per_visit_n))
  cohort_size <- rnorm(10000, cohort_size_mean, cohort_size_sd/sqrt(cohort_size_n))
  phone_cost <- rnorm(10000, phone_cost_mean, phone_cost_sd/sqrt(phone_cost_n))
  laptop_cost <- rnorm(10000, laptop_cost_mean, laptop_cost_sd/sqrt(laptop_cost_n))
  itcost <- rnorm(10000, itcost_mean, itcost_sd/sqrt(itcost_n))
  ehr_cost <- rnorm(10000, ehr_cost_mean, ehr_cost_sd/sqrt(ehr_cost_n))
  depr_cost <- rnorm(10000, depr_cost_mean, depr_cost_sd/sqrt(depr_cost_n))
  space_cost <- rnorm(10000, space_cost_mean, space_cost_sd/sqrt(space_cost_n))
  
  
  
  # Calculate minimum payment per visit
  hours_worked_per_year <- 2080
  supervisor_chw_ratio <- 6
  visits_per_hour <- 1/(visit_length+transport_time_per_visit/4) # 1/4 of visits are in-person
  overhead_by_hr <- (phone_cost/(2080/12)+laptop_cost/4/(2080/12)+itcost/(2080/12)+ehr_cost/(2080/12)+depr_cost*35*transport_time_per_visit*visits_per_hour*hours_worked_per_year/(2080/12)+space_cost/(2080/12))
  min_payment_per_visit <- (chw_wage + chw_overhead + supervisor_wage/supervisor_chw_ratio + supervisor_overhead/supervisor_chw_ratio +overhead_by_hr) / visits_per_hour
  
  # Calculate minimum payment per month per patient
  hours_per_member_per_month <- (hours_worked_per_year/12)/cohort_size
  min_payment_per_month <- (chw_wage + chw_overhead + supervisor_wage/supervisor_chw_ratio + supervisor_overhead/supervisor_chw_ratio +overhead_by_hr) * hours_per_member_per_month
  
  # Calculate mean and 95% confidence intervals around output variables
  min_payment_per_visit_mean <- mean(min_payment_per_visit)
  #min_payment_per_visit_ci <- confint(lm(min_payment_per_visit ~ 1), level = 0.95)
  min_payment_per_visit_lower <- quantile(min_payment_per_visit,c(.025))
  min_payment_per_visit_upper <- quantile(min_payment_per_visit,c(.975))
  
  min_payment_per_month_mean <- mean(min_payment_per_month)
  #min_payment_per_month_ci <- confint(lm(min_payment_per_month ~ 1), level = 0.95)
  min_payment_per_month_lower <- quantile(min_payment_per_month,c(.025))
  min_payment_per_month_upper <- quantile(min_payment_per_month,c(.975))

transport = (depr_cost*35*transport_time_per_visit*visits_per_hour*hours_worked_per_year/(2080/12))/(overhead_by_hr)
# Add results to output data frame
  output_data <- rbind(output_data, data.frame(state = state, 
                                                min_payment_per_visit_mean = min_payment_per_visit_mean,
                                                min_payment_per_visit_lower = min_payment_per_visit_lower,
                                                min_payment_per_visit_upper = min_payment_per_visit_upper,
                                                min_payment_per_month_mean = min_payment_per_month_mean,
                                                min_payment_per_month_lower = min_payment_per_month_lower,
                                                min_payment_per_month_upper = min_payment_per_month_upper,
                                                stringsAsFactors = FALSE))
}

# Write output data to file
write.csv(output_data, "output.csv", row.names = FALSE)

# Format output data for table
table_data <- output_data[, c("state", "min_payment_per_visit_mean", "min_payment_per_visit_lower", "min_payment_per_visit_upper", "min_payment_per_month_mean", "min_payment_per_month_lower", "min_payment_per_month_upper")]
colnames(table_data) <- c("State", "Minimum Payment per Visit", "95% CI (FFS Lower)", "95% CI (FFS Upper)", "Minimum Payment per Month per Patient", "95% CI (PMPM Lower)", "95% CI (PMPM Upper)")
table_data$'Minimum Payment per Visit' <- sprintf("$%.2f", table_data$'Minimum Payment per Visit')
table_data$'95% CI (FFS Lower)' <- sprintf("$%.2f", table_data$'95% CI (FFS Lower)')
table_data$'95% CI (FFS Upper)' <- sprintf("$%.2f", table_data$'95% CI (FFS Upper)')
table_data$'Minimum Payment per Month per Patient' <- sprintf("$%.2f", table_data$'Minimum Payment per Month per Patient')
table_data$'95% CI (PMPM Lower)' <- sprintf("$%.2f", table_data$'95% CI (PMPM Lower)')
table_data$'95% CI (PMPM Upper)' <- sprintf("$%.2f", table_data$'95% CI (PMPM Upper)')

# Create flextable object
ft <- flextable(table_data)

# Add formatting to flextable
ft <- bg(ft, bg = "#D9D9D9", part = "header") # Color header background
ft <- align(ft, align = "center", part = "all") # Center-align all cells
ft <- bold(ft, bold = TRUE, part = "header") # Bold header text
ft <- width(ft, width = c(1, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)) # Set column widths
ft <- height(ft, height = 0.5) # Set row height
ft <- border(ft, part = "all") # Add borders to all cells

# Create Word document and add flextable
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "table.docx")

