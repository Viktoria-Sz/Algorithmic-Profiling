# FUNCTIONS ############################################################################################################

# Heatmap ==============================================================================================================
heatmap <- function(df, var_y, var_x, fill) {
  var_x <- enquo(var_x) # Variable verwendbar machen in ggplot
  var_y <- enquo(var_y) # Variable verwendbar machen in ggplot
  fill <- enquo(fill) # Variable verwendbar machen in ggplot
  
  ggplot(df, aes(y= !!var_y, x = !!var_x, fill = !!fill)) +
    geom_tile() +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) + 
    geom_text(aes(label = round(!!fill, 2))) +
    scale_fill_distiller(palette = "OrRd", direction = -1, limits = c(0,1)) 
}

heatmap_diff <- function(df, var_y, var_x, fill) {
  var_x <- enquo(var_x) # Variable verwendbar machen in ggplot
  var_y <- enquo(var_y) # Variable verwendbar machen in ggplot
  fill <- enquo(fill) # Variable verwendbar machen in ggplot
  
  ggplot(df, aes(y= !!var_y, x = !!var_x, fill = !!fill)) +
    geom_tile() +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) + 
    geom_text(aes(label = round(!!fill, 2))) +
    scale_fill_distiller(palette = "RdYlBu"
                         , direction = 1, limits = c(-0.5,0.5))
}

# Performance Measures =================================================================================================
# sensitivity, recall, hit rate, or true positive rate (TPR)
# -> sens umbenennen in TPR (sens, recall)

# FNR
# TPR = 1 - FNR -> FNR = 1 - TPR
# mutate(FNR = 1-TPR)
FNR <- function(data, truth, estimate, ...) {
  sens(data = data, truth = !! rlang::enquo(truth), estimate = !! rlang::enquo(estimate), ...) %>%
    mutate(.estimate = 1 - .estimate, .metric = "FNR")
}
FNR <- new_class_metric(FNR, "minimize")

# specificity, selectivity or true negative rate (TNR)
# -> spec umbenennen in TNR (spec)

# FPR
# TNR = 1 - FPR -> FPR = 1 - TNR
# mutate(FPR = 1-TNR)
FPR <- function(data, truth, estimate, ...) {
  spec(data = data, truth = !! rlang::enquo(truth), estimate = !! rlang::enquo(estimate), ...) %>%
    mutate(.estimate = 1 - .estimate, .metric = "FPR")
}
FPR <- new_class_metric(FPR, "minimize")

# precision or positive predictive value (PPV)
# -> precision umbenennen in PPV (precision)

# False discrovery rate FDR
# PPV = 1 - FDR -> FDR = 1 - PPV
# mutate(FDR = 1-PPV)
FDR <- function(data, truth, estimate, ...) {
  ppv(data = data, truth = !! rlang::enquo(truth), estimate = !! rlang::enquo(estimate), ...) %>%
    mutate(.estimate = 1 - .estimate, .metric = "FDR")
}
FDR <- new_class_metric(FDR, "minimize")

# Negative predictive value (NPV)
FOR <- function(data, truth, estimate, ...) {
  npv(data = data, truth = !! rlang::enquo(truth), estimate = !! rlang::enquo(estimate), ...) %>%
    mutate(.estimate = 1 - .estimate, .metric = "FOR")
}
FOR <- new_class_metric(FOR, "minimize")

# False  ommission rate
# FOR = 1 - NPV

# All measures combined
performance <- function(df, tasks, label, protected, privileged, unprivileged, measure_list){
  performance_measure_set = metric_set(accuracy 
                                       ,sens # sensitivity, recall, TPR
                                       ,FNR
                                       ,spec # specificity, TNR
                                       ,FPR
                                       ,ppv # Precision, Positive predictive value PPV
                                       ,FDR
                                       ,npv
                                       ,FOR
                                       ,f_meas # fbeta, with beta = 1
                                       ,roc_auc
                                       ,pr_auc
                                       ,detection_prevalence
  )
  
  label <- enquo(label)
  protected <- enquo(protected)
  q_privileged <- rlang::parse_expr(privileged)
  q_unprivileged <- rlang::parse_expr(unprivileged)
  
  for(i in tasks){
    performance_measures = df %>%
      filter(task == i) %>%
      filter(!is.na(!!label)) %>%
      group_by(model) %>%
      performance_measure_set(truth_01, probabilities, estimate = !!label, event_level = 'second')
    
    measures_privileged = df %>%
      filter(task == i & !!protected == privileged) %>%
      filter(!is.na(!!label)) %>%
      group_by(model) %>%
      performance_measure_set(truth_01, probabilities, estimate = !!label, event_level = 'second') %>%
      rename({{privileged}} := .estimate)
    
    measures_unprivileged = df %>%
      filter(task == i & !!protected == unprivileged) %>%
      filter(!is.na(!!label)) %>%
      group_by(model) %>%
      performance_measure_set(truth_01, probabilities, estimate = !!label, event_level = 'second') %>%
      rename({{unprivileged}} := .estimate)
    
    measures = full_join(performance_measures, measures_privileged, by = c("model", ".metric", ".estimator"))
    measures = full_join(measures, measures_unprivileged, by = c("model", ".metric", ".estimator"))
    
    measures = mutate(measures, priv_diff = !!q_privileged - !!q_unprivileged)
    
    # order variables for nicer plotting
    metrics_order <- c("detection_prevalence",
                       "accuracy",
                       "roc_auc",
                       "pr_auc",
                       "sens", # sensitivity, recall, TPR
                       "FNR",
                       "spec", # specificity, TNR
                       "FPR",
                       "ppv", # Precision, Positive predictive value PPV
                       "FDR",
                       "npv",
                       "FOR",
                       "f_meas" # fbeta, with beta = 1
    )
    measures$.metric = factor(measures$.metric, level = metrics_order)

    model_order <- c("Featureless", "OR", "Logistic Regression", "encode.colapply.classif.glmnet.tuned",
                     "Random Forest.tuned", "Decision Tree.tuned",
                     "encode.colapply.classif.xgboost.tuned", "classif.kknn.tuned"
    )
    measures$model = factor(measures$model, level = rev(model_order))

    measures$model <- fct_recode(measures$model,
                                 LogisticRegression =  "Logistic Regression",
                                 PenalizedLR = "encode.colapply.classif.glmnet.tuned",
                                 RandomForest = "Random Forest.tuned",
                                 DecisionTree = "Decision Tree.tuned",
                                 xgboost = "encode.colapply.classif.xgboost.tuned",
                                 KKNN = "classif.kknn.tuned")

    
    # append to list over tasks
    measure_list = append(measure_list, list(measures))
    
  }
  names(measure_list) = tasks
  measure_list

}
# measure_list = list()
# task_list <- unique(df1$task)
# df, tasks, label, protected, privileged, unprivileged, measure_list
# test <- performance(df1, tasks = task_list, label = estimate_0.66, 
#                     protected = gender, privileged = "male", unprivileged = "female", measure_list)
# test <- performance(df1, tasks = task_list, label = estimate_0.66, 
#                     protected = stategroup01, privileged = "AUT", unprivileged = "nAUT", measure_list)


# CI-Plot ==============================================================================================================

# ci_plot
binom_stats <- function(x, ...) {
  x <- x$EMPLOYMENTDAYS[!is.na(x$EMPLOYMENTDAYS)]
  res <- prop.test(x = sum(x == ">=90 Days"), n = length(x), ...)
  data.frame(Proportion  = unname(res$estimate), 
             Lower = res$conf.int[1],
             Upper = res$conf.int[2])
}

jobfind_rate <- mean(data$EMPLOYMENTDAYS == ">=90 Days")


ci_plot <- function(df, var){
  var <- enquo(var)
  
  rates <- df %>%
    group_by(!!var) %>%
    do(binom_stats(.)) %>%
    arrange(Proportion) %>%
    ungroup() #%>%
    # mutate(#var = gsub("GENDER_", "", var),
    #        !!var = reorder(factor(!!var), Proportion))
  
  ggplot(rates, aes(x = !!var, y = Proportion)) +
    geom_hline(yintercept = jobfind_rate, col = "red", alpha = .35, lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = .1) +
    theme(axis.text = element_text(size = 8)) +
    xlab("")
}


# t1-t0 Plot  ==========================================================================================================
df_t0t1 <- function(data, variable, variable_t1){
  var <- enquo(variable)
  var_t1 <- enquo(variable_t1)
  
  var_name <- as_label(var)
  var_t1_name <- as_label(var_t1)
  
  df_t0t1 <- data %>%
    select(!!var, !!var_t1) %>%
    pivot_longer(c(expr(!!var_name), expr(!!var_t1_name)), names_to = "time", values_to= expr(!!var_name), values_drop_na = TRUE)
  
  df_t0t1$time <- factor(df_t0t1$time, levels = c(expr(!!var_name), expr(!!var_t1_name)), labels =  c("t0", "t1"), ordered = FALSE) %>%
    relevel(ref = "t0")
  
  return(df_t0t1)
}
# df_t0t1(data, lottery, lottery_t1)
# plot_prob(df_t0t1(data, lottery, lottery_t1), lottery)

plot_prob <- function(data, variable){
  var <- enquo(variable) # Variable verwendbar machen in ggplot
  
  ggplot(data, aes(x = time, fill = !!var)) +
    geom_bar(position = "fill") +
    theme_bw(base_size = 15)
}
# plot_prob(lottery_df, lottery) 

plot_count <- function(data, variable){
  var <- enquo(variable) # Variable verwendbar machen in ggplot
  
  ggplot(data, aes(x = !!var, fill = time)) +
    geom_bar(position = position_dodge(width = 0.5)) +
    theme_bw(base_size = 15)
}
# plot_count(lottery_df, lottery) 