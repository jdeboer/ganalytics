img_path <- "0f3a2e3f89273dc735506c3d6a6f5fe5.png"
hexSticker::sticker(
  img_path,
  s_x = 1, s_y = .66, s_width = .5, s_height = .5,
  package = "ganalytics", p_size = 24, p_color = "#FFFFFF",
  h_color = "#33BB77", h_fill = "#AA0044",
  filename = devtools::package_file("inst", "figures", "hexicon.png")
)
