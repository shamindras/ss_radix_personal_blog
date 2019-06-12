raw_tv_dat %>%
    dplyr::select(-tt_id) %>%
    DT::datatable(data = .,
                  filter = 'top',
                  # width = "60%",
                  options = list(
                      pageLength = 5))

tv_dat <- raw_tv_dat %>%
            mutate(yr = lubridate::year(date),
                   decade = forcats::as_factor(
                       dplyr::case_when(
                       yr >= 1990  & yr <= 1999 ~ "1990s",
                       yr >= 2000  & yr <= 2009 ~ "2000s",
                       yr >= 2010 ~ "2010s")),
                   title = forcats::as_factor(title))

tv_dat %>%
    # dplyr::filter(decade == "2000s") %>%
    ggplot2::ggplot(data = ., mapping = aes(x = avg_rating,
                                            fill = decade)) +
    ggplot2::geom_histogram(position="identity",
                            alpha = 0.4,
                            binwidth = 0.1) +
    ggplot2::labs(x = "Average IMDb rating") +
    ggplot2::theme(panel.border = element_blank(),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line.x = element_line(colour = "black"),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank()) +
    ggplot2::xlim(0, 10)

tv_dat %>%
    # dplyr::filter(decade == "2000s") %>%
    ggplot2::ggplot(data = ., mapping = aes(x = avg_rating)) +
    ggplot2::geom_histogram(position="identity",
                            alpha = 0.4,
                            binwidth = 0.1) +
    ggplot2::facet_wrap(~ decade) +
    # ggplot2::scale_color_manual(values = c("51C3D7", "51C3D7", "51C3D7")) +
    ggplot2::labs(x = "Average IMDb rating") +
    ggplot2::theme(panel.border = element_blank(),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line.x = element_line(colour = "black"),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank()) +
    ggplot2::xlim(0, 10)

# Let's do the scatter plot
d <- tv_dat %>%
    dplyr::ungroup() %>%
    plotly::highlight_key(., ~title, "Select a TV Show")
p_scatter <- d %>%
    ggplot2::ggplot(data = ., mapping = aes(x = yr,
                                            y = avg_rating,
                                            size = share,
                                            group = title)) +
    ggplot2::geom_point(position = "jitter", alpha = 0.3, color = "green", fill = "green") +
    ggplot2::ylim(c(5.4, 10)) +
    # ggplot2::geom_line(alpha = 0.1, size = 0.5) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(panel.border = element_blank(),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())
p_scatter
plotly::ggplotly(p = p_scatter)
