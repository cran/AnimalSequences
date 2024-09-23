#' Plot the network of elements and covariates based on the long format of sequences
#'
#'
#' @param sequences_long A data frame containing the sequences, with columns for elements and contexts.
#' @param cutoff minimum number of occurrences for which element or covariate should be included
#' @param element A string specifying the column name for elements in the sequences data frame.
#' @param covariate A string specifying the column name for contexts in the sequences data frame.
#' @param n_permutations An integer specifying the number of permutations for the bootstrapping process.
#' @param pvalue cutoff pvalue to include combination
#' @param clusters should clusters be calculated and added?
#'
#' @return plot of bimodal network containing the elements and covariates
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline
#' @importFrom ggraph ggraph create_layout geom_node_text scale_edge_alpha theme_graph geom_edge_fan geom_node_label
#' @importFrom igraph vertex.attributes vertex.attributes<- add_vertices V edge.attributes edge.attributes<- graph_from_data_frame graph.adjacency delete_edges add_vertices get.data.frame bipartite_mapping cluster_fast_greedy modularity V<-
#' @importFrom grDevices rainbow
#' @importFrom graphics layout
#'
#' @export
#'

element_covariate_network <-
  function(sequences_long,
           cutoff = 3,
           element,
           covariate,
           n_permutations = 1000,
           pvalue = 0.01,
           clusters = FALSE) {

    # Create element-covariate combination data

    comparison_object <- element_covariate(sequences_long,
                                 element = element,
                                 covariate = covariate,
                                 n_permutations = n_permutations) %>%
      mutate(prob_increase = .data$probability - .data$expected_probability)


    # create network object
    net.graph <- graph_from_data_frame(
      comparison_object %>%
        filter(.data$p_value <= pvalue) %>%
        filter(.data$count > cutoff &
                 .data$prob_increase > 0) %>%
        select(
          .data$element,
          .data$covariate
        ),
      directed = F,
      vertices = NULL
    ) # create graph
    V(net.graph)$type <-
      bipartite_mapping(net.graph)$type # assign bipartite type as either condition or element

    # set colors and shapes
    V(net.graph)$color <-
      ifelse(V(net.graph)$type, "salmon", "lightblue") # color set if there are no clusters
    V(net.graph)$shape <- ifelse(V(net.graph)$type, "bold", "italic")

    # test for clusters
    net.un <- net.graph
    net.community <-
      cluster_fast_greedy(net.un) # other clustering algorithms exist, eg walktrap
    modular <-
      round(modularity(net.community), 2) # modularity measure. Above 0.3 is good modularity
    net.com <- data.frame(
      element = net.community$names,
      community = net.community$membership
    )
    color <- rainbow(length(unique(net.com$community)))
    if (clusters) {
      V(net.graph)$color <-
        color[net.com$community]
    } # color set if there are no clusters}

    all.layout <- create_layout(net.graph,
                                layout = "igraph",
                                algorithm = "nicely"
    ) # create basic layout that all the graphs will share, so they are symmetrical

    p.occurrence <- ggraph(all.layout) +
      geom_node_text(
        mapping = aes(
          color = .data$color,
          label = .data$name,
          size = 20,
          fontface = .data$shape
        ),
        show.legend = FALSE
      ) +
      scale_edge_alpha(guide = "none") +
      theme_graph(base_family = "sans") + # if this is removed, there is bizarrely a constant message telling us that the font does not exist
      ggtitle('Element-Covariate combinations') +
      # make edges, labels, and arrows
      geom_edge_fan(
        mapping = aes(colour = .data$type),
        label_size = 4,
        arrow = NULL,
        colour = "grey",
        fontface = "bold",
        label_dodge = unit(2, "mm"),
        angle_calc = "along",
        show.legend = F
      ) +
      geom_node_label(
        mapping = aes(
          label = .data$name,
          color = .data$color,
          size = 20,
          fontface = .data$shape
        ),
        show.legend = FALSE
      )
    return(p.occurrence)
  }
