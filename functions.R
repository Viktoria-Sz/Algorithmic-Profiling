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
    scale_fill_distiller(palette = "RdYlBu", direction = 1) 
}

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