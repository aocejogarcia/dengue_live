snd_theme <- function(
    p,
    title = NULL,
    subtitle = NULL,
    x_title = NULL,
    y_title = NULL,
    legend = TRUE,
    show_grid = TRUE
) {
  
  # ------------------------------------------------------------
  # SND / Gobierno de México inspired palette
  # ------------------------------------------------------------
  
  snd_burgundy <- "#611232"
  snd_gold     <- "#B38E5D"
  snd_gray     <- "#666666"
  snd_grid     <- "#E9E9E9"
  snd_white    <- "#FFFFFF"
  
  # ------------------------------------------------------------
  # Title
  # ------------------------------------------------------------
  
  title_text <- title
  
  if (!is.null(subtitle)) {
    title_text <- paste0(
      "<b>", title, "</b>",
      "<br>",
      "<span style='font-size:12px;color:",
      snd_gray,
      ";'>",
      subtitle,
      "</span>"
    )
  }
  
  # ------------------------------------------------------------
  # Apply theme
  # ------------------------------------------------------------
  
  p %>%
    layout(
      
      # General
      font = list(
        family = "Montserrat, Arial, sans-serif",
        color = "#333333",
        size = 13
      ),
      
      paper_bgcolor = snd_white,
      plot_bgcolor  = snd_white,
      
      # Title
      title = list(
        text = title_text,
        font = list(
          family = "Montserrat, Arial, sans-serif",
          size = 18,
          color = snd_burgundy
        ),
        x = 0,
        xanchor = "left",
        y = 0.98,
        yanchor = "top"
      ),
      
      # X axis
      xaxis = list(
        title = list(
          text = x_title,
          font = list(
            color = snd_burgundy,
            size = 13
          )
        ),
        showgrid = show_grid,
        gridcolor = snd_grid,
        gridwidth = 1,
        showline = TRUE,
        linecolor = snd_gold,
        linewidth = 1,
        ticks = "outside",
        tickcolor = snd_gray,
        tickfont = list(
          color = "#333333",
          size = 12
        ),
        zeroline = FALSE
      ),
      
      # Y axis
      yaxis = list(
        title = list(
          text = y_title,
          font = list(
            color = snd_burgundy,
            size = 13
          )
        ),
        showgrid = show_grid,
        gridcolor = snd_grid,
        gridwidth = 1,
        showline = TRUE,
        linecolor = snd_gold,
        linewidth = 1,
        ticks = "outside",
        tickcolor = snd_gray,
        tickfont = list(
          color = "#333333",
          size = 12
        ),
        zeroline = TRUE,
        zerolinecolor = "#999999",
        zerolinewidth = 1
      ),
      
      # Legend
      legend = list(
        visible = legend,
        orientation = "h",
        x = 0,
        y = 1.08,
        xanchor = "left",
        yanchor = "bottom",
        font = list(
          family = "Montserrat, Arial, sans-serif",
          size = 12,
          color = "#333333"
        ),
        bgcolor = "rgba(255,255,255,0.85)"
      ),
      
      # Hover
      hoverlabel = list(
        bgcolor = snd_burgundy,
        bordercolor = snd_burgundy,
        font = list(
          family = "Montserrat, Arial, sans-serif",
          color = snd_white,
          size = 12
        )
      ),
      
      # Margins
      margin = list(
        l = 65,
        r = 65,
        t = 85,
        b = 65
      )
    )
}