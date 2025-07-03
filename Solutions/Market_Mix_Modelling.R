# =====================================================================================
#  ELECTROMART MARKETING BUDGET OPTIMIZATION USING ROBYN MMM
#  This script implements Marketing Mix Modeling with hypothesis testing to optimize
#  ElectroMart's marketing budget allocation across channels and product categories.
# =====================================================================================

#First setup Nevergrad https://github.com/facebookexperimental/Robyn/blob/main/demo/install_nevergrad.R

# Directory where you want to export results to (will create new folders)
robyn_directory <- "~/Desktop/GCR"

# ==== PART 1: SETUP AND DATA LOADING =================================================

# Set up virtual env every time you open
library(reticulate)
virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)

# Enable multi-core processing
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

# Load required libraries with error handling
required_libraries <- c("Robyn", "lubridate", "lmtest","dplyr")
for (lib in required_libraries) {
  if (!require(lib, character.only = TRUE, quietly = TRUE)) {
    message(paste("Installing package:", lib))
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

# Load the monthly data
monthly_data <- read.csv("monthly.csv")

# Fix column names by replacing spaces with underscores
colnames(monthly_data) <- gsub(" ", "_", colnames(monthly_data))
colnames(monthly_data) <- gsub("\\.", "_", colnames(monthly_data))

# Display the structure of the data to understand variables
str(monthly_data)

# Check for missing values
colSums(is.na(monthly_data))

# ==== PART 2: DATA PREPROCESSING =====================================================

# Create proper date variable (required by Robyn)
monthly_data$DATE <- as.Date(paste(monthly_data$Year, monthly_data$Month, "01", sep="-"))

# Identify marketing channels for analysis - use exact column names
marketing_channels <- c("TV", "Digital", "Sponsorship", "Content_Marketing", 
                        "Online_Marketing", "Affiliates", "SEM", "Radio", "Other")

# ==== PART 3: EXPLORATORY DATA ANALYSIS WITH HYPOTHESIS TESTING ======================

# ------ 3.1: Relationship between Revenue & Advertisement Spends ---------------------

# Hypothesis: Marketing channel spends have significant correlation with GMV
# Test significance of correlations between channels and Total_GMV
channel_tests <- data.frame(
  Channel = character(),
  Correlation = numeric(),
  p_value = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

for(channel in marketing_channels) {
  # Skip if the column doesn't exist
  if (!channel %in% colnames(monthly_data)) {
    warning(paste("Channel", channel, "not found in data. Skipping."))
    next
  }
  
  # Perform correlation test - using the correct column name from the dataframe
  cor_test <- cor.test(monthly_data$Total_GMV, monthly_data[[channel]], 
                       method="pearson")
  
  # Store results
  channel_tests <- rbind(channel_tests, data.frame(
    Channel = channel,
    Correlation = cor_test$estimate,
    p_value = cor_test$p.value,
    Significant = ifelse(cor_test$p.value < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  ))
}

# Sort by absolute correlation strength
if (nrow(channel_tests) > 0) {
  channel_tests <- channel_tests[order(abs(channel_tests$Correlation), decreasing = TRUE), ]
  print("Channel correlation with GMV and statistical significance:")
  print(channel_tests)
  
  # Visualize correlations
  if (nrow(channel_tests) > 0) {
    par(mar=c(5, 10, 4, 2)) # Adjust margins for better display
    barplot(channel_tests$Correlation, 
            names.arg = channel_tests$Channel,
            main = "Correlation Between Marketing Channels and Revenue",
            xlab = "Correlation Coefficient", 
            horiz = TRUE,
            col = ifelse(channel_tests$Significant == "Yes", "darkblue", "lightblue"),
            las = 1,
            xlim = c(min(channel_tests$Correlation) - 0.1, max(channel_tests$Correlation) + 0.1))
    legend("bottomright", legend = c("Significant", "Not Significant"), 
           fill = c("darkblue", "lightblue"))
  }
}

# ------ 3.2: Holiday Impact Analysis -------------------------------------------------

# Check if both columns exist with the right names
if ("Has_Holiday" %in% colnames(monthly_data) && "Holiday_Percentage" %in% colnames(monthly_data)) {
  # Convert Has Holiday to factor - using the correct column name with backticks
  monthly_data$Has_Holiday_Factor <- as.factor(monthly_data[["Has_Holiday"]])
  
  # Check the number of observations in each group
  print(table(monthly_data$Has_Holiday_Factor))
  
  # Wilcoxon test with exact = FALSE to avoid warning
  wilcox_result <- wilcox.test(Total_GMV ~ Has_Holiday_Factor, data = monthly_data, exact = FALSE)
  print("Wilcoxon test for holiday impact:")
  print(wilcox_result)
  
  # For the regression analysis, use correct column names with backticks
  holiday_model <- lm(Total_GMV ~ `Has_Holiday` * `Holiday_Percentage`, data = monthly_data)
  print("Holiday impact regression model:")
  print(summary(holiday_model))
  
  # Create scatterplot ensuring vectors are the same length
  # First ensure the vectors have the same length by checking for NAs
  valid_indices <- complete.cases(monthly_data[, c("Holiday_Percentage", "Total_GMV", "Has_Holiday")])
  
  if (sum(valid_indices) > 0) {
    plot(monthly_data$`Holiday_Percentage`[valid_indices], 
         monthly_data$Total_GMV[valid_indices],
         main = "Relationship Between Holiday Percentage and GMV",
         xlab = "Holiday Percentage", 
         ylab = "Total GMV",
         col = ifelse(monthly_data$`Has_Holiday`[valid_indices] == 1, "red", "blue"),
         pch = 19)
    abline(lm(Total_GMV ~ `Holiday_Percentage`, data = monthly_data[valid_indices, ]), col = "black")
    legend("topright", legend = c("Holiday", "No Holiday"), 
           col = c("red", "blue"), pch = 19)
  } else {
    warning("No complete cases for Holiday Percentage vs Total_GMV plot")
  }
  
  # Create composite holiday intensity variable with correct column names
  monthly_data$Holiday_Intensity <- monthly_data$`Has_Holiday` * monthly_data$`Holiday_Percentage`
  holiday_intensity_model <- lm(Total_GMV ~ Holiday_Intensity, data = monthly_data)
  print("Holiday intensity impact:")
  print(summary(holiday_intensity_model))
} else {
  warning("Required holiday columns not found. Skipping holiday analysis.")
}

# ------ 3.3: NPS (Customer Satisfaction) Impact Analysis -----------------------------

if ("NPS" %in% colnames(monthly_data)) {
  # Hypothesis: Customer satisfaction (NPS) predicts future revenue
  # Create lagged NPS variable
  monthly_data <- monthly_data[order(monthly_data$DATE), ]
  monthly_data$NPS_lag1 <- c(NA, head(monthly_data$NPS, -1))
  
  # Test correlation between lagged NPS and revenue
  nps_cor_test <- cor.test(monthly_data$NPS_lag1, monthly_data$Total_GMV, 
                           method = "pearson", use = "complete.obs")
  print("Correlation between lagged NPS and GMV:")
  print(nps_cor_test)
  
  # Regression model to quantify impact
  nps_model <- lm(Total_GMV ~ NPS_lag1, data = monthly_data)
  print("NPS impact on GMV:")
  print(summary(nps_model))
  
  # Plot the relationship
  valid_indices <- !is.na(monthly_data$NPS_lag1) & !is.na(monthly_data$Total_GMV)
  if (sum(valid_indices) > 0) {
    plot(monthly_data$NPS_lag1[valid_indices], monthly_data$Total_GMV[valid_indices],
         main = "Impact of Previous Month's NPS on Current Month's GMV",
         xlab = "NPS (Previous Month)", 
         ylab = "Total GMV",
         col = "orange",
         pch = 19)
    abline(nps_model, col = "black")
  } else {
    warning("No complete cases for NPS_lag1 vs Total_GMV plot")
  }
} else {
  warning("NPS column not found. Skipping NPS analysis.")
}

# ------ 3.4: Product Category Analysis -----------------------------------------------

# Identify product categories from column names
product_categories <- c("Camera", "CameraAccessory", "EntertainmentSmall", "GameCDDVD", "GamingHardware")
valid_categories <- product_categories[product_categories %in% colnames(monthly_data)]

if (length(valid_categories) > 0) {
  # Hypothesis: Product categories contribute differently to revenue
  # Calculate category contribution percentages
  for (category in valid_categories) {
    pct_col <- paste0(category, "_Pct")
    monthly_data[[pct_col]] <- monthly_data[[category]] / monthly_data$Total_GMV * 100
  }
  
  # Calculate means for valid categories
  category_means <- sapply(paste0(valid_categories, "_Pct"), function(col) mean(monthly_data[[col]], na.rm = TRUE))
  names(category_means) <- valid_categories
  
  print("Mean contribution of each product category (%):")
  print(category_means)
  
  # Visualize category contributions
  barplot(category_means, 
          main = "Average Contribution of Product Categories to GMV",
          xlab = "Product Category", 
          ylab = "Contribution (%)",
          col = rainbow(length(category_means)),
          las = 2)
  
  # Create long-format data for ANOVA
  if (length(valid_categories) > 1) {  # Need at least 2 categories for ANOVA
    # Reshape data for ANOVA
    category_data <- c()
    category_names <- c()
    
    for (category in valid_categories) {
      category_data <- c(category_data, monthly_data[[category]])
      category_names <- c(category_names, rep(category, nrow(monthly_data)))
    }
    
    category_long <- data.frame(
      DATE = rep(monthly_data$DATE, length(valid_categories)),
      Category = factor(category_names),
      Revenue = category_data
    )
    
    # ANOVA to test for significant differences between categories
    category_anova <- aov(Revenue ~ Category, data = category_long)
    print("ANOVA for category revenue differences:")
    print(summary(category_anova))
  } else {
    warning("At least 2 valid categories needed for ANOVA. Skipping.")
  }
} else {
  warning("No valid product categories found. Skipping category analysis.")
}

# ------ 3.5: Trend and Seasonality Analysis ------------------------------------------

# Hypothesis: There is significant trend and seasonality in GMV
# Test for trend significance
trend_test <- lm(Total_GMV ~ as.numeric(DATE), data = monthly_data)
print("Linear trend test:")
print(summary(trend_test))

# Plot the trend
plot(monthly_data$DATE, monthly_data$Total_GMV, type = "l",
     main = "GMV Trend Over Time",
     xlab = "Date", 
     ylab = "Total GMV",
     col = "blue")
abline(trend_test, col = "red")
legend("topleft", legend = c("Actual GMV", "Trend Line"), 
       col = c("blue", "red"), lty = 1)

# Time series analysis - handle the decomposition error
monthly_data <- monthly_data[order(monthly_data$DATE), ]
gmv_ts <- ts(monthly_data$Total_GMV, frequency = 12)

# Check if we have enough data for decomposition (need 2+ complete cycles)
if (length(gmv_ts) >= 24) {
  decomposed <- decompose(gmv_ts)
  plot(decomposed, main = "Time Series Decomposition of GMV")
} else {
  # Use alternative approach for shorter time series
  print("Not enough data for time series decomposition. Using simple trend plotting instead.")
  
  # Use month as a factor for seasonality test
  monthly_data$Month_Factor <- factor(monthly_data$Month)
  
  # Simple visual test for monthly patterns
  if (nrow(monthly_data) >= 12) {
    monthly_means <- aggregate(Total_GMV ~ Month, data = monthly_data, FUN = mean)
    barplot(monthly_means$Total_GMV, 
            names.arg = monthly_means$Month,
            main = "Average GMV by Month",
            xlab = "Month", 
            ylab = "Average GMV",
            col = "lightblue")
  }
}

# ------ 3.6: Weather Impact Analysis -------------------------------------------------

weather_vars <- c("tavg", "prcp", "wspd", "pres")
valid_weather_vars <- weather_vars[weather_vars %in% colnames(monthly_data)]

if (length(valid_weather_vars) >= 2) {  # Need at least 2 variables for PCA
  # Create principal components from weather variables
  weather_data <- monthly_data[, valid_weather_vars]
  
  # Check for missing values
  if (any(is.na(weather_data))) {
    weather_data <- na.omit(weather_data)
    warning("Missing values in weather data. Some rows were removed.")
  }
  
  if (nrow(weather_data) > 0) {
    weather_pca <- prcomp(weather_data, scale = TRUE)
    print("PCA summary for weather variables:")
    print(summary(weather_pca))
    
    # Add principal components to dataset
    if (nrow(weather_pca$x) == nrow(monthly_data)) {
      monthly_data$Weather_PC1 <- weather_pca$x[, 1]
      if (ncol(weather_pca$x) >= 2) {
        monthly_data$Weather_PC2 <- weather_pca$x[, 2]
      }
      
      # Test significance of weather components
      weather_model <- lm(Total_GMV ~ Weather_PC1, data = monthly_data)
      print("Weather impact on GMV:")
      print(summary(weather_model))
      
      # Plot relationship with first principal component
      plot(monthly_data$Weather_PC1, monthly_data$Total_GMV,
           main = "Relationship Between Weather (PC1) and GMV",
           xlab = "Weather Principal Component 1", 
           ylab = "Total GMV",
           col = "darkgreen",
           pch = 19)
      abline(lm(Total_GMV ~ Weather_PC1, data = monthly_data), col = "black")
    } else {
      warning("PCA results dimensions don't match data. Skipping weather analysis.")
    }
  } else {
    warning("No complete cases for weather analysis after removing NAs.")
  }
} else {
  warning("Not enough weather variables found. Skipping weather analysis.")
}

# ==== PART 4: FEATURE ENGINEERING ====================================================

# Calculate KPIs for analysis based on requirements
# Calculate ROAS for each marketing channel (with error handling)
for (channel in marketing_channels) {
  if (channel %in% colnames(monthly_data)) {
    roas_col <- paste0(gsub(" ", "_", channel), "_ROAS")
    monthly_data[[roas_col]] <- ifelse(monthly_data[[channel]] > 0, 
                                       monthly_data$Total_GMV / monthly_data[[channel]], NA)
  }
}

# Create sales intensity metric if columns exist
if (all(c("Sales_Days", "Sales_Percentage") %in% colnames(monthly_data))) {
  monthly_data$Sales_Intensity <- monthly_data$`Sales_Days` * monthly_data$`Sales_Percentage`
} else {
  warning("Sales Days or Sales Percentage columns not found. Skipping sales intensity calculation.")
}

# Create month-over-month growth
monthly_data <- monthly_data[order(monthly_data$DATE), ]
monthly_data$GMV_growth <- c(NA, diff(monthly_data$Total_GMV) / head(monthly_data$Total_GMV, -1) * 100)

# ==== PART 5: STATISTICAL VARIABLE SELECTION =========================================

# Since stepwise regression fails with AIC = -infinity, use correlation-based selection
# Identify potential predictors
potential_predictors <- c(marketing_channels, "NPS", "Stock_Index", "Holiday_Intensity")
if ("Weather_PC1" %in% colnames(monthly_data)) 
  potential_predictors <- c(potential_predictors, "Weather_PC1")
if ("Sales_Intensity" %in% colnames(monthly_data)) 
  potential_predictors <- c(potential_predictors, "Sales_Intensity")

# Filter to columns that actually exist
valid_predictors <- potential_predictors[sapply(potential_predictors, function(col) {
  col %in% colnames(monthly_data) || 
    (grepl(" ", col) && gsub(" ", ".", col) %in% colnames(monthly_data))
})]

# Calculate correlations with GMV
correlations <- numeric(length(valid_predictors))
names(correlations) <- valid_predictors

for(i in seq_along(valid_predictors)) {
  pred <- valid_predictors[i]
  if (pred %in% colnames(monthly_data)) {
    col_data <- monthly_data[[pred]]
  } else if (gsub(" ", ".", pred) %in% colnames(monthly_data)) {
    col_data <- monthly_data[[gsub(" ", ".", pred)]]
  } else {
    col_data <- NA
  }
  
  if (is.numeric(col_data)) {
    correlations[i] <- cor(monthly_data$Total_GMV, col_data, use = "complete.obs")
  } else {
    correlations[i] <- NA
  }
}

# Remove NA values
correlations <- correlations[!is.na(correlations)]

# Sort and select top correlated variables
sorted_predictors <- names(sort(abs(correlations), decreasing = TRUE))
top_predictors <- head(sorted_predictors, min(4, length(sorted_predictors)))  # Limit to 4 variables

# Build a model with top correlated predictors
if (length(top_predictors) > 0) {
  # Create formula with backticks for column names with spaces
  formula_terms <- sapply(top_predictors, function(x) {
    if (grepl(" ", x)) paste0("`", x, "`") else x
  })
  
  formula_str <- paste("Total_GMV ~", paste(formula_terms, collapse = " + "))
  
  # Build the model
  reduced_model <- try(lm(formula_str, data = monthly_data))
  
  if (!inherits(reduced_model, "try-error")) {
    print("Reduced model with top correlated predictors:")
    print(summary(reduced_model))
    
    # Extract significant variables
    model_summary <- summary(reduced_model)
    significant_vars <- rownames(model_summary$coefficients)[
      which(model_summary$coefficients[, "Pr(>|t|)"] < 0.1 & 
              rownames(model_summary$coefficients) != "(Intercept)")
    ]
    
    print("Variables selected as significant predictors:")
    print(significant_vars)
    
    # Select top marketing channels based on correlation and significance
    marketing_channels_in_model <- intersect(top_predictors, gsub("\\.", " ", marketing_channels))
    top_channels <- head(marketing_channels_in_model, min(4, length(marketing_channels_in_model)))
    
    print("Top marketing channels selected for modeling:")
    print(top_channels)
  } else {
    warning("Failed to build reduced model. Using top correlations instead.")
    top_channels <- head(intersect(sorted_predictors, gsub("\\.", " ", marketing_channels)), 
                         min(4, length(intersect(sorted_predictors, gsub("\\.", " ", marketing_channels)))))
    print("Top correlated marketing channels:")
    print(top_channels)
  }
} else {
  warning("No valid predictors found for modeling.")
}

# ==== PART 6: INDIVIDUAL CHANNEL EFFECTIVENESS TESTING ===============================

# Test hypothesis that each channel has non-zero effect on GMV
channel_tests <- list()

for(channel in marketing_channels) {
  if (channel %in% colnames(monthly_data)) {
    # Create model for each channel
    formula_str <- paste("Total_GMV ~", paste0("`", channel, "`"))
    channel_model <- try(lm(formula_str, data = monthly_data))
    
    if (!inherits(channel_model, "try-error")) {
      # Store results
      channel_tests[[channel]] <- list(
        coefficient = coef(channel_model)[2],
        p_value = summary(channel_model)$coefficients[2, "Pr(>|t|)"],
        significant = summary(channel_model)$coefficients[2, "Pr(>|t|)"] < 0.05,
        r_squared = summary(channel_model)$r.squared
      )
    }
  }
}

if (length(channel_tests) > 0) {
  # Convert to data frame for easier viewing
  channel_test_df <- do.call(rbind, lapply(names(channel_tests), function(name) {
    data.frame(
      Channel = name,
      Coefficient = channel_tests[[name]]$coefficient,
      P_Value = channel_tests[[name]]$p_value,
      Significant = channel_tests[[name]]$significant,
      R_Squared = channel_tests[[name]]$r_squared,
      stringsAsFactors = FALSE
    )
  }))
  
  print("Individual channel effectiveness tests:")
  print(channel_test_df)
  
  # Visualize channel coefficients
  if (nrow(channel_test_df) > 0) {
    par(mar=c(5, 10, 4, 2)) # Adjust margins for better display
    barplot(channel_test_df$Coefficient, 
            names.arg = channel_test_df$Channel,
            main = "Individual Channel Impact on GMV",
            xlab = "GMV Impact Coefficient", 
            horiz = TRUE,
            col = ifelse(channel_test_df$Significant, "darkblue", "lightblue"),
            las = 1)
    legend("bottomright", legend = c("Significant", "Not Significant"), 
           fill = c("darkblue", "lightblue"))
  }
} else {
  warning("No channel tests could be performed.")
}

# ==== PART 7: PRODUCT CATEGORY TARGETING ANALYSIS ===================================

# Calculate correlations between product categories and marketing channels
product_categories <- c("Camera", "CameraAccessory", "EntertainmentSmall", 
                        "GameCDDVD", "GamingHardware")

# Create correlation matrix
category_channel_corr <- cor(
  monthly_data[, product_categories],
  monthly_data[, marketing_channels],
  use = "complete.obs"
)

# Find best channels for each category (highest correlation)
category_recommendations <- data.frame(
  Category = product_categories,
  Top_Channel = apply(category_channel_corr, 1, function(x) marketing_channels[which.max(x)]),
  Second_Channel = sapply(1:nrow(category_channel_corr), function(i) 
    marketing_channels[order(category_channel_corr[i,], decreasing = TRUE)[2]]),
  Correlation_Max = apply(category_channel_corr, 1, max)
)

print("Product category marketing channel recommendations:")
print(category_recommendations)

# Test statistical significance of these correlations
significant_pairs <- data.frame(
  Category = character(),
  Channel = character(),
  Correlation = numeric(),
  P_Value = numeric(),
  Significant = character()
)

for(category in product_categories) {
  for(channel in marketing_channels) {
    cor_test <- cor.test(monthly_data[[category]], monthly_data[[channel]], 
                         method = "pearson")
    
    significant_pairs <- rbind(significant_pairs, data.frame(
      Category = category,
      Channel = channel,
      Correlation = cor_test$estimate,
      P_Value = cor_test$p.value,
      Significant = ifelse(cor_test$p.value < 0.05, "Yes", "No")
    ))
  }
}

# Filter to show only significant relationships
significant_pairs <- significant_pairs[significant_pairs$Significant == "Yes", ]
significant_pairs <- significant_pairs[order(abs(significant_pairs$Correlation), 
                                             decreasing = TRUE), ]
print("Statistically significant category-channel relationships:")
print(significant_pairs)

# ==== PART 8: SIMPLIFIED ROBYN MODEL SETUP ===========================================

# Fix column names by replacing spaces with underscores
colnames(monthly_data) <- make.names(colnames(monthly_data), unique = TRUE)

# Update marketing_channels to match the new column names
marketing_channels <- make.names(marketing_channels, unique = TRUE)

# Example fix - convert factor to numeric when needed
if (is.factor(monthly_data$Has_Holiday_Factor)) {
  monthly_data$Has_Holiday_Numeric <- as.numeric(as.character(monthly_data$Has_Holiday_Factor))
}

# Check if Robyn is installed and loaded
if (requireNamespace("Robyn", quietly = TRUE)) {
  # Define variables for Robyn based on correlation analysis
  if (exists("top_channels") && length(top_channels) > 0) {
    # Select context variables based on correlations
    context_vars <- c("Holiday_Intensity", "Stock Index")
    context_vars_selected <- context_vars[context_vars %in% colnames(monthly_data)]
    
    # Set up InputCollect for Robyn - simplified approach
    tryCatch({
      InputCollect <- robyn_inputs(
        dt_input = monthly_data,
        date_var = "DATE",
        dep_var = "Total_GMV",
        dep_var_type = "revenue",
        pprophet_vars = c("trend", "season", "holiday"),
        prophet_country = "CA",   # Canada
        paid_media_spends = top_channels, # Using selected top channels
        organic_vars = if ("NPS" %in% colnames(monthly_data)) "NPS" else NULL,
        context_vars = context_vars_selected,
        window_start = min(monthly_data$DATE),
        window_end = max(monthly_data$DATE),
        adstock = "geometric",
        ts_validation = TRUE
      )
      
      # Fix hyperparameters naming
      hyperparameters <- list(
        NPS_thetas = c(0.1, 0.9),
        NPS_alphas = c(0.5, 3), 
        NPS_gammas = c(0.3, 1)
      )
      
      # Get the exact names as they appear in top_channels
      for (channel in top_channels) {
        # Remove spaces and special characters for parameter names
        clean_channel <- gsub("[^a-zA-Z0-9]", "_", channel)
        
        # Get the exact column name from the dataset
        exact_column <- names(monthly_data)[which(tolower(gsub("[^a-zA-Z0-9]", "", names(monthly_data))) == 
                                                    tolower(gsub("[^a-zA-Z0-9]", "", channel)))]
        
        if (length(exact_column) > 0) {
          # Set theta bounds (decay rate)
          theta_name <- paste0(exact_column, "_thetas")
          hyperparameters[[theta_name]] <- c(0, 0.3)
          
          # Set gamma bounds (saturation)
          gamma_name <- paste0( exact_column, "_gammas")
          hyperparameters[[gamma_name]] <- c(0.3, 1)
          
          alpha_name <- paste0( exact_column, "_alphas")
          hyperparameters[[alpha_name]] <- c(0.5, 3)
        }
      }
      
      # Add train_size explicitly
      hyperparameters$train_size <- c(0.5, 0.8)
      
      # Print the hyperparameter names to verify
      print(names(hyperparameters))
      
      # Complete the InputCollect with hyperparameters
      InputCollect <- robyn_inputs(
        InputCollect = InputCollect,
        hyperparameters = hyperparameters
      )
      
      print("Robyn model input configuration:")
      print(InputCollect)
      
      # Note: Actual model execution is commented out to avoid potential errors
      # with the small dataset
      
      print("Robyn model setup completed, but execution is skipped.")
      print("To run the model, uncomment the robyn_run section in the code.")
      
    }, error = function(e) {
      warning(paste("Error in Robyn setup:", e$message))
    })
  } else {
    warning("No top channels identified for Robyn modeling.")
  }
} else {
  warning("Robyn package not available. Skipping Robyn modeling sections.")
}

# ==== PART 9: ROBYN MODEL EXECUTION ==================================================

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 3000, 
  trials = 3, 
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature to add more flexibility
)
print(OutputModels)

MOO <- robyn_converge(
  OutputModels,
  n_cuts = 30,        # Increase from default 20
  sd_qtref = 4,       # Adjust from default 3
  med_lowb = 3,        # Increase from default 2
  nrmse_win = c(0.01, 0.99)  # Slightly more aggressive than default
)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
InputCollect, OutputModels,
pareto_fronts = 1, # automatically pick how many pareto-fronts to fill min_candidates (100)
min_candidates = 1, # top pareto models for clustering. Default to 100
calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
csv_out = "pareto", # "pareto", "all", or NULL (for none)
clusters = FALSE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
export = TRUE, # this will create files locally
plot_folder = robyn_directory, # path for plots exports and files creation
plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)

# Initialize an empty dataframe to store model metrics
model_scores <- data.frame(
  model_id = character(),
  nrmse_test = numeric(),
  decomp_rssd = numeric(),
  r2_test = numeric(),
  score = numeric(),
  stringsAsFactors = FALSE
)

# Loop through all available models
for (model_id in OutputCollect$resultHypParam$solID) {
  # Get model metrics
  model_metrics <- OutputCollect$resultHypParam[OutputCollect$resultHypParam$solID == model_id, ]
  
  # Extract key metrics
  nrmse_test <- model_metrics$nrmse_test
  decomp_rssd <- model_metrics$decomp.rssd
  r2_test <- model_metrics$rsq_test
  
  # Calculate score: (1-NRMSE_test)*0.4 + (1-DECOMP.RSSD)*0.4 + R²_test*0.2
  score <- (1 - nrmse_test) * 0.4 + (1 - decomp_rssd) * 0.4 + r2_test * 0.2
  
  # Add to dataframe
  model_scores <- rbind(
    model_scores,
    data.frame(
      model_id = model_id,
      nrmse_test = nrmse_test,
      decomp_rssd = decomp_rssd,
      r2_test = r2_test,
      score = score
    )
  )
}

# Sort by score in descending order
model_scores <- model_scores[order(-model_scores$score), ]

# Print all models with their scores
print(model_scores)

# Select the best model
best_model <- model_scores$model_id[1]
cat("\nBest model based on combined score:", best_model, "\n")

# Use this model for your analysis
select_model <- best_model

# Generate one-pager for the selected model
robyn_onepagers(InputCollect, OutputCollect, select_model)


# ==== PART 10: MODEL RESULTS ANALYSIS =================================================

# Debug what robyn_response returns
test_response <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = top_channels[1],
  metric_value = sum(monthly_data[[top_channels[1]]], na.rm = TRUE)
)

# Print the structure to see available properties
str(test_response)

# Calculate channel effectiveness
channel_effectiveness <- data.frame(
  Channel = character(), 
  Current_Spend = numeric(), 
  Marginal_ROI = numeric(), 
  Recommendation = character()
)

for(channel in top_channels) {
  # Calculate current total spend
  current_spend <- sum(monthly_data[[channel]], na.rm = TRUE)
  
  # Get current response - capture the full response object first
  response_obj_current <- robyn_response(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = select_model,
    metric_name = channel,
    metric_value = current_spend
  )
  
  # Get response with additional spend
  response_obj_additional <- robyn_response(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = select_model,
    metric_name = channel,
    metric_value = current_spend + 1000
  )
  
  # Check if responses were generated successfully
  if(!is.null(response_obj_current) && !is.null(response_obj_additional)) {
    # Try different properties based on what's available
    if("response_total" %in% names(response_obj_current)) {
      response_current <- response_obj_current$response_total
      response_additional <- response_obj_additional$response_total
    } else if("sim_mean_response" %in% names(response_obj_current)) {
      response_current <- response_obj_current$sim_mean_response
      response_additional <- response_obj_additional$sim_mean_response
    } else {
      # Skip this channel if we can't get response values
      warning(paste("Skipping channel", channel, "- couldn't find response values"))
      next
    }
    
    # Calculate marginal ROI
    marginal_roi <- (response_additional - response_current) / 1000
    
    # Add to dataframe
    channel_effectiveness <- rbind(
      channel_effectiveness,
      data.frame(
        Channel = channel,
        Current_Spend = current_spend,
        Marginal_ROI = marginal_roi,
        Recommendation = ifelse(marginal_roi > 1, "Increase", "Decrease")
      )
    )
  } else {
    warning(paste("Skipping channel", channel, "- robyn_response returned NULL"))
  }
}


# Sort by marginal ROI
channel_effectiveness <- channel_effectiveness[order(channel_effectiveness$Marginal_ROI, 
                                                     decreasing = TRUE), ]
print("Channel effectiveness analysis:")
print(channel_effectiveness)

# ==== PART 11: BUDGET ALLOCATION OPTIMIZATION ========================================

# Calculate optimal allocation using robyn_allocator for max_response 
# Scenario "max_response": 
#"What's the max. return given certain spend?"
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_12",  # Last 12 periods
  channel_constr_low = rep(0.4, length(top_channels)),  # Min 60% of current spend
  channel_constr_up = rep(1.4, length(top_channels)),   # Max 130% of current spend
  scenario = "max_response"
)
# Print optimization results
print("Budget allocation optimization results:")
plot(AllocatorCollect)
print(AllocatorCollect)

# Scenario "target_efficiency": "How much to spend to hit ROAS or CPA of x?"
# Example 3: Use default ROAS target for revenue or CPA target for conversion
# Two default ROAS targets: 0.8x of initial ROAS as well as ROAS = 1
# Two default CPA targets: 1.2x and 2.4x of the initial CPA
AllocatorCollectTwo <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  channel_constr_low = 0.1,
  channel_constr_up = 10,
  date_range = NULL, # Default: "all" available dates
  scenario = "target_efficiency",
  target_value = 5 # Customize target ROAS or CPA value
)
print(AllocatorCollectTwo)
plot(AllocatorCollectTwo)

# ==== PART 12: SENSITIVITY ANALYSIS =============================================

# Perform sensitivity analysis (using "target_efficiency" scenario)
sensitivity_analysis <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_12",
  scenario = "target_efficiency",  # <--- VALID OPTION
  channel_constr_low = c(0.8, 0.7, 0.6),  # Match length to paid_media_spends
  channel_constr_up = c(1.2, 1.3, 1.4)    # Match length to paid_media_spends
)

print("Sensitivity analysis results with target efficiency:")
print(sensitivity_analysis)

# Maximize response without efficiency constraints
sensitivity_analysis <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_12",
  scenario = "max_response",  # <--- Alternative valid option
  channel_constr_low = rep(0.5, 3),  # -50% for all 3 channels
  channel_constr_up = rep(0.5, 3)    # +50% for all 3 channels
)
 
print("Sensitivity analysis results with max response:")
print(sensitivity_analysis)

# ==== PART 13: SATURATION CURVE ANALYSIS =============================================

# Generate and analyze saturation curves for each significant channel
for(channel in top_channels) {
  # Get response curve
  response_curve <- robyn_response(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = select_model,
    metric_name = channel
  )
  
  # Print curve
  print(paste("Response curve for", channel))
  print(response_curve$plot)
}
# ================PART 14:BUDGET REALLOCATION ANALYSIS===================
# Errors Fixed
realloc_results <- data.frame(
  Channel = AllocatorCollect$dt_optimOut$channels,
  Original_Budget = round(AllocatorCollect$dt_optimOut$initSpendUnit, 0),
  New_Budget = round(AllocatorCollect$dt_optimOut$optmSpendUnit, 0),
  Budget_Shift = round(AllocatorCollect$dt_optimOut$optmSpendUnit - 
                         AllocatorCollect$dt_optimOut$initSpendUnit, 0),
  Revenue_Lift = round(AllocatorCollect$dt_optimOut$optmResponseUnit - 
                         AllocatorCollect$dt_optimOut$initResponseUnit, 0)
) %>%
  mutate(
    Budget_Change_Pct = ifelse(Original_Budget == 0, NA,
                               round((New_Budget / Original_Budget - 1) * 100, 1)),
    Revenue_per_Dollar = round(Revenue_Lift / Budget_Shift, 2)
  )

# Calculate current total spend across selected channels
current_total_spend <- sum(sapply(top_channels, function(x) sum(monthly_data[[x]], na.rm = TRUE)))

# Run allocator with FIXED total budget
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_12",
  scenario = "max_response",
  channel_constr_low = rep(0.4, length(top_channels)),  # Allow min 40% of original spend
  channel_constr_up = rep(1.4, length(top_channels)),   # Allow max 140% of original spend
  total_budget = current_total_spend,  # Critical: Lock total spend to current level
  export = TRUE
)

# Verify total spend remains constant
optimized_total_spend <- sum(AllocatorCollect$dt_optimOut$optmSpendUnit)
cat("Budget Validation:\n",
    "Original Total:", format(current_total_spend, big.mark = ","), "\n",
    "Optimized Total:", format(optimized_total_spend, big.mark = ","), "\n",
    "Difference:", format(optimized_total_spend - current_total_spend, big.mark = ","), "\n\n")

# Calculate revenue impact
current_revenue <- sum(AllocatorCollect$dt_optimOut$initResponseUnit)
optimized_revenue <- sum(AllocatorCollect$dt_optimOut$optmResponseUnit)
revenue_lift <- optimized_revenue - current_revenue

# Create allocation table with checks
realloc_results <- data.frame(
  Channel = AllocatorCollect$dt_optimOut$channels,
  Original_Budget = round(AllocatorCollect$dt_optimOut$initSpendUnit, 0),
  New_Budget = round(AllocatorCollect$dt_optimOut$optmSpendUnit, 0),
  Budget_Shift = round(AllocatorCollect$dt_optimOut$optmSpendUnit - 
                         AllocatorCollect$dt_optimOut$initSpendUnit, 0),
  Revenue_Lift = round(AllocatorCollect$dt_optimOut$optmResponseUnit - 
                         AllocatorCollect$dt_optimOut$initResponseUnit, 0)
) %>% 
  mutate(
    Budget_Change_Pct = ifelse(Original_Budget == 0, NA,
                               round((New_Budget / Original_Budget - 1) * 100, 1)),
    Revenue_per_Dollar = round(Revenue_Lift / Budget_Shift, 2)
  )

# Remove channels with no budget change
realloc_results <- realloc_results[realloc_results$Budget_Shift != 0, ]

# Print results with validation
print("Budget-Neutral Reallocation Results:")
print(realloc_results)

cat("\nRevenue Impact Analysis:",
    "\nOriginal 12-Month Revenue:", format(current_revenue, big.mark = ","),
    "\nOptimized Revenue:", format(optimized_revenue, big.mark = ","),
    "\nAbsolute Lift:", format(revenue_lift, big.mark = ","),
    paste0("(+", round((revenue_lift/current_revenue)*100, 1), "%)"))

