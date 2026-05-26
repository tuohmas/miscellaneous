# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi
#
# Script for creating an affiliation network of Finnish interest groups
#
## PREPARATIONS ################################################################

# Load packages
pacman::p_load(dplyr,       # with data handling
               tidyr,       # with data handling
               readr,       # with data handling
               backbone,    # with bipartite projections
               checkpoint,  # with reproduceability
               vroom,       # with csv reading
               igraph,      # with social network analysis
               qgraph,      # with addtional SNA
               GGally)      # with corrplot

options(scipen = 999)

# Load data
raw_data <- read_csv("data/interest_group_memberships.csv") # Replace


# raw_data <- read.csv("data/interest_group_memberships.csv", sep = ";")

### WRANGLE DATA ###############################################################

raw_data %>%
  # Change names
  mutate(interest_group = ifelse(interest_group == "Tekno",
                                 "Teknologiateollisuus", interest_group),
         # Change separators to clean up the code
         interest_group = gsub(" ","_", interest_group),
         interest_group = gsub("/","_", interest_group),
         # Yearly boards
         board = paste0(interest_group,"_",year))
  # select(name, rank_among_top_earners, board, membership_type)

grouped_data <- raw_data %>%
  group_by(name, rank_among_top_earners, board) %>%
  summarise(memberships = n())

# Replace NA ranks_among_top_earners with 10,000
grouped_data[is.na(grouped_data)] <- 10000

glimpse(grouped_data)

# Turn into wide format
data_wider <-
  spread(grouped_data, key = board, value = memberships)

# Replace empty values with zeros
data_wider[is.na(data_wider)] <- 0

glimpse(data_wider)

## MAKE GRAPHS #################################################################

# Make bipartite graph
bipartite_data <- grouped_data[,c("name", "board")]

B <- graph.data.frame(bipartite_data, directed = FALSE)

bipartite.mapping(B)

V(B)$type <- bipartite_mapping(B)$type

V(B)$color <- ifelse(V(B)$type, "salmon", "lightblue")
V(B)$shape <- ifelse(V(B)$type, "square", "circle")
E(B)$color <- "lightgray"

plot(B, vertex.size = 2, vertex.label.cex = 0.8,
     layout = layout_with_fr,
     vertex.label = ifelse(V(B)$type, V(B)$name, NA)
     # vertex.label.color = "black"
     )

# Replace NA ranks_among_top_earners with 10,000
grouped_data[is.na(grouped_data)] <- 10000

glimpse(grouped_data)

# Turn into wide format
data_wider <-
  spread(grouped_data, key = board, value = memberships)

# Replace empty values with zeros
data_wider[is.na(data_wider)] <- 0

glimpse(data_wider)

View(node_attributes)

# Build a dataframe for node attributes: how many interest groups, and wealth
node_attributes <- raw_data %>%
  select(name, rank_among_top_earners, interest_group) %>%

  mutate(rank_among_top_earners = ifelse(is.na(rank_among_top_earners),
                  10000, rank_among_top_earners)) %>%

  group_by(name, rank_among_top_earners, interest_group) %>%

  summarise(memberships = n()) %>%

  spread(key = interest_group, value = memberships) %>%

  replace(is.na(.), 0)

glimpse(node_attributes)

names(node_attributes)

# Count memberships in different interest groups (non-zero rowsums)
node_attributes$memberships <-
  rowSums(node_attributes[3:13]!=0) # Count interest group columns

table(node_attributes$memberships)

node_attributes <-
  node_attributes %>% select(name, rank_among_top_earners, memberships)

# Normalize earning based ranks: 1 for top and 0.001 for lowest ones
node_attributes <-
  node_attributes %>%
  mutate(normalized_rank = (rank_among_top_earners - 10000) / (1 - 10000)) %>%

      # 1 - ((rank_among_top_earners - min(rank_among_top_earners)) /
      # (max(rank_among_top_earners) - min(rank_among_top_earners)))) %>%
  # For rank of zeros
  mutate(normalized_rank = ifelse(normalized_rank == 0, 0.001, normalized_rank))

glimpse(node_attributes)

# Matrix multiplication
data_wider <- data_wider %>%
  ungroup() %>%
  group_by(name) %>%
  select(-rank_among_top_earners) %>% as.data.frame()

mat_data_wider <- data_wider %>% select(-name) %>% as.matrix()
rownames(mat_data_wider) <- data_wider$name

glimpse(mat_data_wider)

# Construct bipartite projections
P_artifacts <- t(mat_data_wider) %*% mat_data_wider # transpose columns
P_agents <- mat_data_wider %*% t(mat_data_wider) # transpose rows

View(P_agents)
View(P_artifacts)

# Construct weighted and unweighted graphs
G <- graph_from_adjacency_matrix(
  P_agents, mode = "undirected", weighted = TRUE, diag = FALSE)

G2 <- graph_from_adjacency_matrix(
  P_artifacts, mode = "undirected", weighted = TRUE, diag = FALSE)

unweighted_G <-
  graph_from_adjacency_matrix(P_agents, mode = "undirected", weighted = NULL,
                                 diag = FALSE) %>% simplify()

vcount(G)
ecount(G)
graph.density(G)

vcount(G2)
ecount(G2)
graph.density(G2)

plot(G, vertex.size = 4, vertex.label = NA, edge.width = E(G)$weight)

multilevel <- multilevel.community(G2)
fg <- fastgreedy.community(G2)
optimal <- optimal.community(G2)

colors <- rainbow(max(membership(multilevel)))
colors <- rainbow(max(membership(fg)))
colors <- rainbow(max(membership(optimal)))

plot(G2, vertex.size = 4, vertex.label.cex = 0.4, edge.color = "gray90",
     vertex.label.family = "sans", edge.alpha = 0.1,
     edge.width = E(G_artifacts)$weight / 100,
     # vertex.color = "salmon"
     vertex.color = colors[membership(fg)])



# Centrality measures, agents
V(G)$betweenness <- betweenness(G, directed = FALSE,
                                      normalized = TRUE)

V(G)$eigencentrality <- eigen_centrality(G)$vector

V(G)$degree <- degree(G, mode = "total")
V(G)$w_degree <- strength(G, mode = "total")

# Centrality measures, artifacts
V(G2)$betweenness <- betweenness(G2, directed = FALSE,
                                normalized = TRUE)

V(G2)$eigencentrality <- eigen_centrality(G2)$vector

V(G2)$degree <- degree(G2, mode = "total")
V(G2)$w_degree <- strength(G2, mode = "total")

network_centrality_artifacts <-
  data.frame(name = V(G2)$name,
             betweenness = V(G2)$betweenness,
             eigencentrality = V(G2)$eigencentrality,
             degree_centrality = V(G2)$degree) %>%
  arrange(desc(betweenness))

View(network_centrality_artifacts)

plot(G2)

# Ego network, AH
vid_ah <- V(B)[V(B)$name == "Antti Herlin"]

ego_ah <- ego(B, order = 2, nodes = vid_ah, mode = c("all"))
ego_ah <- subgraph(B, ego_ah[[1]])

ego_size(B, V(B)[25], order = 1, mode = "all") # How many interest_groups
ego_size(B, V(B)[25], order = 2, mode = "all") # + all agents affiliated to them

plot(ego_ah, vertex.size = 4, layout = layout_with_fr,
     vertex.label.cex = 0.4, vertex.label.family = "sans",
     vertex.color = ifelse(V(ego_ah)$name == "Antti Herlin",
                           "gold", V(ego_ah)$color))

# Ego network, RS
vid_rs <- V(B)[V(B)$name == "Risto Siilasmaa"]

ego_rs <- ego(B, order = 2, nodes = vid_rs, mode = c("all"))
ego_rs <- subgraph(B, ego_rs[[1]])

ego_size(B, vid_rs, order = 1, mode = "all") # How many interest_groups
ego_size(B, vid_rs, order = 2, mode = "all") # + all agents affiliated to them

plot(ego_ah, vertex.size = 4, layout = layout_with_fr,
     vertex.label.cex = 0.4, vertex.label.family = "sans",
     vertex.color = ifelse(V(ego_ah)$name == "Risto Siilasmaa",
                           "gold", V(ego_ah)$color))

# V(G)[betweenness == max(betweenness)]
# neighbors(G, V(G)[betweenness == max(betweenness)])
# max(V(G)$betweenness)

# Add network attributes
vertex_order <- get.vertex.attribute(G, "name")
node_attributes <- node_attributes[order(G),]

V(G)$linkage <- node_attributes$memberships
V(G)$earning_rank <- node_attributes$normalized_rank

V(G)$rank_color <- ifelse(V(G)$linkage == 1, "gray80", "plum")
V(G)$rank_color <- ifelse(V(G)$linkage > 1, "gold", V(G)$rank_color)
V(G)$rank_color <- ifelse(V(G)$linkage > 2, "tomato", V(G)$rank_color)

network_centrality <- data.frame(name = V(G)$name,
                                 betweenness = V(G)$betweenness,
                                 eigencentrality = V(G)$eigencentrality,
                                 degree_centrality = V(G)$degree,
                                 memberships = V(G)$linkage,
                                 earnings_normalized = V(G)$earning_rank) %>%
  arrange(desc(betweenness))

View(network_centrality)

# Correlations between different centrality measures + earnings and memberships
ggpairs(network_centrality[,2:ncol(network_centrality)],
        lower = list(combo = wrap("facethist", bins = 20)))

# Clustering, initial
optimal_clustering <- cluster_optimal(G)
multilevel_community <- multilevel.community(G)

# Highlight longest shortes path
nodes_diameter <- get.diameter(G)
edges_incident <- get.edge.ids(G, nodes_diameter)

# V(G)[nodes_diameter]$color<-"orange" # Set the nodes on the diameter to be green

V(G)$color <- membership(multilevel_community)
E(G)$color <- "grey90" # Set default edge color
E(G)[edges_incident]$color <- "orange" # Set the edges on the diameter to be green

V(G)

plot(G,
     layout = layout_nicely,
     vertex.size = V(G)$earning_rank * 4,
     vertex.label = ifelse(V(G)$linkage > 1, V(G)$name, NA),
     edge.color = E(G)$color,
     vertex.color = V(G)$rank_color)
     # edge.width = E(G)$weight
     # mark.groups = multilevel_community

legend("topright", legend =
         c("Hallituspaikka yhdessä järjestössä","Hallituspaikka kahdessa eri järjestössä (linkkaaja)",
           "Hallituspaikka kolmessa tai useammassa eri järjestössä (superlinkkaaja)"), pch=21,
       col="#777777", pt.bg=c("gray80","gold","tomato"),
       pt.cex=2, cex=.8, bty="n", ncol=1,)

# Count componenents
count_components(G)

# Subgraph: select giant component only
components <- igraph::clusters(G, mode="weak")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(G)[components$membership == biggest_cluster_id]

G_giant <- induced_subgraph(G, vert_ids)

E(G)$color <- "grey80"

plot(G_giant,
     # layout_with_fr(G_giant),
     vertex.size = 6, vertex.label = NA, edge.width = E(G_giant)$weight)

# Calculate average path length and average
avg_path_length <- average.path.length(G)
clustering_coefficient <- transitivity(G_giant, type = "undirected")
avg_neighbours <- ego_size(G_giant, 1, V(G_giant)) %>% mean()

# Compute the small-worldness index (Humphries & Gurney, 2008) relying on the
# global transitity of the network (Newman, 2003) and on its average shortest
# path length.
smallworldness(G_giant, 100)

# Compare index to The Watts-Strogatz small-world model
sample_smallworld(
  dim = 1, size = vcount(G_giant), nei = avg_neighbours, p = 0.05)
  # %>%
  # average.path.length()
  # transitivity()
  # smallworldness(100)

# Backbone: disparity
disparity <- disparity(P, alpha = 0.05, class = "igraph")

vcount(disparity)
ecount(disparity)

backbone <- osdsm(G_giant, alpha = 0.05, class = "igraph",
                 narrative = TRUE)

vcount(backbone)
ecount(backbone)

# Get vertex order and set vertex attributes to that order
vertex_order <- get.vertex.attribute(G, "name")
linkage <- linkage[order(vertex_order),]

V(G)$linkage <- linkage$membership_counts
V(G)$earning_rank <- linkage$normalized_rank

V(G)$rank_color <- ifelse(V(G)$linkage == 1, "gray80", "plum")
V(G)$rank_color <- ifelse(V(G)$linkage > 1, "gold", V(G)$rank_color)
V(G)$rank_color <- ifelse(V(G)$linkage > 2, "tomato", V(G)$rank_color)

inc.edges <- incident_edges(G, V(G)[linkage > 2], mode="all")
# inc.edges <- incident(G, V(G)[linkage > 2], mode="all")

# unlist <- unlist(inc.edges)

ecol <- rep("gray90", ecount(G))
# ecol[inc.edges$`Päivi Leiwo`] <- "orange"

esize <- rep(1, ecount(G))
# esize[inc.edges$`Päivi Leiwo`] <- 4

# vlabel <- rep(NA, vcount(G))
# vlabel[V(G)$name == "Päivi Leiwo"] <- 1

plot(G,
     # layout_with_gem(G),
     vertex.size = V(G)$earning_rank * 4, vertex.label = NA,
     edge.color= ecol, vertex.color = V(G)$rank_color, edge.size=esize)

legend("bottomright", legend=
  c("Paikka 1:ssä eri järjestössä","Paikka 2:ssa eri järjestössä (linkkaaja)",
    "Paikka 3:ssa tai useammassa eri järjestössä (superlinkkaaja)"), pch=21,
  col="#777777", pt.bg=c("gray80","gold","tomato"),
  pt.cex=2, cex=.8, bty="n", ncol=1,)

cut_1 <- 1
cut_2 <- linkage[which.min(abs(linkage$rank_among_top_earners - 1000)),"normalized_rank"][[1]]
cut_3 <- linkage[which.min(abs(linkage$rank_among_top_earners - 2500)),"normalized_rank"][[1]]
cut_4 <- linkage[which.min(abs(linkage$rank_among_top_earners - 5000)),"normalized_rank"][[1]]
cut_5 <- linkage[which.min(abs(linkage$rank_among_top_earners - 7500)),"normalized_rank"][[1]]
cut_6 <- 0.01
cut_7 <- linkage[which.min(abs(linkage$rank_among_top_earners - 10000)),"normalized_rank"][[1]]

# Size different earning ranks
size_cuts <- c(cut_1, cut_2, cut_3, cut_4, cut_5, cut_6, cut_7)
size_legend <- c("1. (suurituloisin)","1 000.","2 500.","5 000.","7 500.", "10 000.", "< 10 000.")

length(size_cut)
length(size_legend)

size_cut_scale <- size_cut*4

legend('topright', legend = size_legend, pt.cex = size_cut_scale, title = "Sijoitus suurituloisimpien suomalaisten joukossa")

a <- legend('topright',legend = size_legend,
            pt.cex = size_cut_scale/200, col='white',
            pch=21, pt.bg='white', title = "Sijoitus suurituloisimpien suomalaisten joukossa")


x <- (a$text$x + a$rect$left) / 2
y <- a$text$y

symbols(x,y,circles = size_cut_scale/200, inches = FALSE, add = TRUE, bg = 'gold')

# Backbone: Get vertex order and set vertex attributes to that order
bb_vertex_order <- get.vertex.attribute(backbone, "name")
linkage <- linkage[order(bb_vertex_order),]

V(backbone)$linkage <- linkage$membership_counts
V(backbone)$earning_rank <- linkage$normalized_rank

V(backbone)$rank_color <- ifelse(V(disparity)$linkage == 1, "gray80", "plum")
V(backbone)$rank_color <- ifelse(V(disparity)$linkage > 1, "gold", V(disparity)$rank_color)
V(backbone)$rank_color <- ifelse(V(disparity)$linkage > 2, "tomato", V(disparity)$rank_color)

ecol <- rep("gray90", ecount(backbone))
esize <- rep(1, ecount(backbone))

plot(backbone,
     vertex.size = V(backbone)$earning_rank * 4, vertex.label = NA,
     edge.color= ecol, vertex.color = V(backbone)$rank_color, edge.size=esize)

# Disparity: Get vertex order and set vertex attributes to that order
disparity_vertex_order <- get.vertex.attribute(backbone, "name")
linkage <- linkage[order(disparity_vertex_order),]

V(disparity)$linkage <- linkage$membership_counts
V(disparity)$earning_rank <- linkage$normalized_rank

V(disparity)$rank_color <- ifelse(V(disparity)$linkage == 1, "gray80", "plum")
V(disparity)$rank_color <- ifelse(V(disparity)$linkage > 1, "gold", V(disparity)$rank_color)
V(disparity)$rank_color <- ifelse(V(disparity)$linkage > 2, "tomato", V(disparity)$rank_color)

ecol <- rep("gray90", ecount(disparity))
esize <- rep(1, ecount(disparity))

plot(disparity,
     vertex.size = V(disparity)$earning_rank * 4, vertex.label = NA,
     edge.color= ecol, vertex.color = V(disparity)$rank_color, edge.size=esize)

V(disparity)$betweeness <- betweenness(unweighted_G, normalized = TRUE)
V(disparity)$degree <- degree(unweighted_G, normalized = TRUE)
V(disparity)$eigencentrality <- eigen_centrality(unweighted_G, scale = TRUE)$vector

plot(disparity,
     vertex.size = V(disparity)$degree * 9,
     vertex.label = NA,
     edge.color= ecol,
     vertex.color = V(disparity)$rank_color,
     edge.size=esize)
