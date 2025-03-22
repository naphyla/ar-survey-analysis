bar_graph <- function(
    df,
    variable,
    palette,
    text_col = "black"
){
  df |> 
    group_by({{ variable }}) |> 
    summarise(n = n()) |> 
    ungroup() |> 
    mutate(Proportion = n/sum(n)) |> 
    ggplot(aes(x = Proportion, y = "", fill = {{ variable }})) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format(decimal.mark = ",", accuracy = 1)) +
    geom_text(aes(label = paste0(round(Proportion * 100, 0), "%")),
              position = position_stack(vjust = 0.5), colour = text_col) +
    scale_fill_brewer(palette = palette, labels = function(x) str_wrap(x, width = 15),
                      guide = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      legend.position="top"
    ) +
    labs(
      x = "", 
      y = ""
    )
}


grouped_bar_braph <- function(
  df,
  variable, 
  grouping_var,
  palette,
  text_col = "black"
  ){
  df |> 
    group_by({{ grouping_var }}, {{ variable }}) |> 
    summarise(n = n()) |> 
    group_by(table_2025) |> 
    mutate(Proportion = n/sum(n)) |> 
    ggplot(aes(x = Proportion, y = table_2025, fill = {{ variable }})) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format(decimal.mark = ",", accuracy = 1)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 5)) + 
    geom_text(aes(label = paste0(round(Proportion * 100, 0), "%")),
              position = position_stack(vjust = 0.5), colour = text_col) +
    scale_fill_brewer(palette = palette, labels = function(x) str_wrap(x, width = 15),
                      guide = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      legend.position="top"
    ) + 
    labs(
      x = "",
      y = ""
    )
}













