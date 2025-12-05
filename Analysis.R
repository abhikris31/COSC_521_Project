#Creating 
library(readxl)
# --- Define the base data directory (must exist in your GitHub repo root) ---
data_dir <- "data" 
# Assuming structure: your-repo-root/data/bipartite-dataframes/...

# Define the subdirectory path for clarity
bipartite_subdir <- "bipartite-dataframes"

# ----------------------------------------------------------------------
# Read files for Industrial Policy (IP)
# ----------------------------------------------------------------------
# IP Country-Policy data
df_ip_cp <- read_excel(file.path(data_dir, 
                                 bipartite_subdir, 
                                 "Industrial Policy data_Country_Policy.xlsx"))

# IP Policy-Product data
df_ip_pp <- read_excel(file.path(data_dir, 
                                 bipartite_subdir, 
                                 "Industrial Policy data_Policy_Product.xlsx"))

# IP Country-Product data
df_ip_cpr <- read_excel(file.path(data_dir, 
                                  bipartite_subdir, 
                                  "Industrial Policy data_Country_Product.xlsx"))


# ----------------------------------------------------------------------
# Read files for Non-Industrial Policy (Non-IP)
# ----------------------------------------------------------------------
# Non-IP Country-Policy data
df_nonip_cp <- read_excel(file.path(data_dir, 
                                    bipartite_subdir, 
                                    "Non Industrial Policy data_Country_Policy.xlsx"))

# Non-IP Policy-Product data
df_nonip_pp <- read_excel(file.path(data_dir, 
                                    bipartite_subdir, 
                                    "Non Industrial Policy data_Policy_Product.xlsx"))

# Non-IP Country-Product data
df_nonip_cpr <- read_excel(file.path(data_dir, 
                                     bipartite_subdir, 
                                     "Non Industrial Policy data_Country_Product.xlsx"))
head(df_ip_pp)

# Replace "NAN" or "nan" in df_ip_cp
df_ip_cp[df_ip_cp == "NAN"] <- NA
df_ip_cp[df_ip_cp == "nan"] <- NA

df_ip_pp[df_ip_pp == "NAN"] <- NA
df_ip_pp[df_ip_pp == "nan"] <- NA

df_ip_cpr[df_ip_cpr == "NAN"] <- NA
df_ip_cpr[df_ip_cpr == "nan"] <- NA

df_nonip_cp[df_nonip_cp == "NAN"] <- NA
df_nonip_cp[df_nonip_cp == "nan"] <- NA

df_nonip_pp[df_nonip_pp == "NAN"] <- NA
df_nonip_pp[df_nonip_pp == "nan"] <- NA

df_nonip_cpr[df_nonip_cpr == "NAN"] <- NA
df_nonip_cpr[df_nonip_cpr == "nan"] <- NA


# For Country-Policy
sum(is.na(df_ip_cp))  # total number of NA values

# For Policy-Product
sum(is.na(df_ip_pp))

# For Country-Product
sum(is.na(df_ip_cpr))

#Analysis for Objective 4
# Defining Node Attributes

#Reading Attribute Files
data_dir <- "data" 
# Assuming structure: your-repo-root/data/bipartite-dataframes/...

# Define the subdirectory path for clarity
complexity_subdir <- "complexity-files"

df_eci <- read_excel(file.path(data_dir, 
                                 complexity_subdir, 
                                 "df_eci.xlsx"))

df_pci <- read_excel(file.path(data_dir, 
                                 complexity_subdir, 
                                 "df_pci.xlsx"))

library(igraph)

# Create the bipartite graph from your long-format dataframe
g_ip_cp <- graph_from_data_frame(df_ip_cp, directed = FALSE)

V(g_ip_cp)$type <- V(g_ip_cp)$name %in% df_ip_cp$policy

library(stringr)

# Trim spaces and unify case
V(g_ip_cp)$name <- str_trim(V(g_ip_cp)$name)
df_eci$country <- str_trim(df_eci$country)

V(g_ip_cp)$name <- str_to_title(V(g_ip_cp)$name)
df_eci$country <- str_to_title(df_eci$country)

country_nodes <- V(g_ip_cp)[!V(g_ip_cp)$type]

idx <- match(V(g_ip_cp)$name[country_nodes], df_eci$country)

unmatched <- country_nodes[is.na(idx)]
g_ip_cp <- delete_vertices(g_ip_cp, unmatched)

country_nodes <- V(g_ip_cp)[!V(g_ip_cp)$type]   # updated graph
idx <- match(V(g_ip_cp)$name[country_nodes], df_eci$country)

V(g_ip_cp)$eci <- NA
V(g_ip_cp)$eci[country_nodes] <- df_eci$eci[idx]

deg_p <- degree(g_ip_cp, v = V(g_ip_cp)[type == TRUE])
deg_p

deg_p_sorted <- sort(deg_p)

#Plotting
png("policy_degree_sorted.png", width = 1500, height = 800)

par(mar = c(5, 20, 5, 2))

barplot(deg_p_sorted,
        horiz = TRUE,
        names.arg = names(deg_p_sorted),
        main = "Degree of Policy Nodes",
        xlab = "Degree",
        ylab = "",
        las = 1,
        xlim = c(0, max(deg_p_sorted) + 5),
        cex.names = 1.2,    # increase size of category labels (policy names)
        cex.axis = 1.2,     # increase tick label size (numbers)
        cex.lab  = 1.4      # increase x-axis label size)
        )
dev.off()

policy_nodes <- V(g_ip_cp)[type == TRUE]

avg_eci <- sapply(policy_nodes, function(p) {
  c_nodes <- neighbors(g_ip_cp, p)
  mean(V(g_ip_cp)[c_nodes]$eci)
})

avg_eci

#Plotting
png("Correlation_plot.png", width = 1500, height = 800)

par(mar = c(5, 10, 5, 2))

plot(deg_p, avg_eci,
     xlab = "Degree of policy",
     ylab = "Average ECI of implementing countries",
     #cex.names = 1.2,    # increase size of category labels (policy names)
     cex.axis = 1.8,     # increase tick label size (numbers)
     cex.lab  = 1.8,      # increase x-axis label size)
     pch = 19)

# Add regression line
abline(lm(avg_eci~deg_p), col = "red", lwd = 2)
r <- cor(deg_p, avg_eci)
  legend("topright", legend = paste0("r = ", round(r, 2)), bty = "n",cex = 2)

dev.off()
#Sort them in decreasing order
#Plot correlation between these

#One Mode Projection

proj <- bipartite_projection(g_ip_cp)

g_policy <- proj[[2]]  # second element = policy projection

sim_mat <- similarity(g_policy, method = "jaccard")

# 3. Add row and column names (important!)
rownames(sim_mat) <- V(g_policy)$name
colnames(sim_mat) <- V(g_policy)$name

#Weighting edges with Jaccard similarity
E(g_policy)$weight <- sim_mat[as.matrix(as_edgelist(g_policy, names= TRUE))]

comm <- cluster_louvain(g_policy, weights = E(g_policy)$weight)
membership <- membership(comm)

print(length(unique(membership)))

# Compute community-level metrics using bipartite network
community_metrics_bipartite <- sapply(unique(membership), function(c) {
  
  # Policies in this community
  pols <- V(g_policy)[membership == c]$name
  
  # Degree in bipartite network: number of countries adopting each policy
  degrees <- degree(g_ip_cp, v = pols)
  avg_degree <- mean(degrees)
  
  # Countries adopting these policies
  countries <- unique(unlist(neighbors(g_ip_cp, pols)))
  countries <- countries[V(g_ip_cp)[countries]$type == FALSE]  # keep only countries
  
  # Average ECI of countries
  avg_eci <- mean(V(g_ip_cp)[countries]$eci)
  
  c(avg_degree = avg_degree, avg_ECI = avg_eci)
})

# Transpose for readability
community_metrics_bipartite <- t(community_metrics_bipartite)
rownames(community_metrics_bipartite) <- paste0("Community_", unique(membership))
community_metrics_bipartite

library(RColorBrewer)

# 1. Set community colors
num_communities <- length(unique(membership))
colors <- brewer.pal(min(num_communities, 12), "Set3")
V(g_policy)$color <- colors[membership]

# 2. Set edge widths proportional to weights
E(g_policy)$width <- E(g_policy)$weight * .5  # adjust multiplier for visibility

# 3. Set fixed node size
V(g_policy)$size <- 15  # all nodes same size

# 4. Plot using a layout
set.seed(42)
layout <- layout_with_kk(g_policy, maxiter = 1000)

png("Community Plot_plot.png", width = 1000, height = 800)

plot(g_policy,
     layout = layout,
     vertex.label = V(g_policy)$name,
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     edge.color = "grey50")

# Optional: add legend
legend("topright", legend = paste("Community", 1:num_communities),
       fill = colors[1:num_communities], bty = "n")

dev.off()

policy_names = V(g_ip_cp)[type == TRUE]$name

policy_metrics <- data.frame(
  policy = policy_names,
  avg_ECI = avg_eci,  # assuming you stored it here
  degree = deg_p,    # bipartite degree
  community = membership
)

# Split by community
policy_by_comm <- split(policy_metrics, policy_metrics$community)

policy_by_comm

png("Correlation Plot_communitywise.png", width = 1500, height = 800)

par(mfrow = c(1, 2),mar = c(5,5,4,2))  # 3 plots side by side

for (i in 1:length(policy_by_comm)) {
  df <- policy_by_comm[[i]]
  
  plot(df$degree, df$avg_ECI,
       main = paste("Community", i),
       xlab = "Degree (Bipartite)",
       ylab = "Average ECI",
       pch = 19, col = "steelblue",
       cex = 2,
       cex.main = 2,
       cex.axis = 1.8,     # increase tick label size (numbers)
       cex.lab  = 1.8)
  
  # Add regression line
  abline(lm(avg_ECI ~ degree, data = df), col = "red", lwd = 2)
  
  # Optional: print correlation
  r <- cor(df$degree, df$avg_ECI)
  legend("topright", legend = paste0("r = ", round(r, 2)), bty = "n",cex = 2)
}

dev.off()

deg_c <- degree(g_ip_cp, v = V(g_ip_cp)[type == FALSE])

#Plotting Correlation of country's degree with its ECI
png("Correlation_plot_country.png", width = 1500, height = 800)

par(mar = c(5, 10, 5, 2))

plot(deg_c, V(g_ip_cp)[type == FALSE]$eci,
     xlab = "Degree of country",
     ylab = "ECI",
     #cex.names = 1.2,    # increase size of category labels (policy names)
     cex.axis = 1.8,     # increase tick label size (numbers)
     cex.lab  = 1.8,      # increase x-axis label size)
     pch = 19)

# Add regression line
abline(lm(V(g_ip_cp)[type == FALSE]$eci ~ deg_c), col = "red", lwd = 2)

r <- cor(deg_c, V(g_ip_cp)[type == FALSE]$eci)
  legend("topright", legend = paste0("r = ", round(r, 2)), bty = "n",cex = 2)

dev.off()

#Analysis for Objective 2
# Defining Node Attributes

library(igraph)

# Create the bipartite graph from your long-format dataframe
g_ip_pp <- graph_from_data_frame(df_ip_pp, directed = FALSE)

V(g_ip_pp)$type <- V(g_ip_pp)$name %in% df_ip_pp$policy

library(stringr)

# Trim spaces and unify case
V(g_ip_pp)$name <- str_trim(V(g_ip_pp)$name)
df_pci$product <- str_trim(df_pci$product)

V(g_ip_pp)$name <- str_to_title(V(g_ip_pp)$name)
df_pci$product <- str_to_title(df_pci$product)

product_nodes <- V(g_ip_pp)[!V(g_ip_pp)$type]

idx <- match(V(g_ip_pp)$name[product_nodes], df_pci$product)

unmatched <- product_nodes[is.na(idx)]
g_ip_pp <- delete_vertices(g_ip_pp, unmatched)

country_nodes <- V(g_ip_pp)[!V(g_ip_pp)$type]   # updated graph
idx <- match(V(g_ip_pp)$name[product_nodes], df_pci$product)

V(g_ip_pp)$pci <- NA
V(g_ip_pp)$pci[product_nodes] <- df_pci$pci[idx]

deg_p <- degree(g_ip_pp, v = V(g_ip_pp)[type == TRUE])
deg_p

deg_p_sorted <- sort(deg_p)

#Plotting
png("policy_degree_sorted_Policy Product.png", width = 1500, height = 800)

par(mar = c(5, 20, 5, 2))

barplot(deg_p_sorted,
        horiz = TRUE,
        names.arg = names(deg_p_sorted),
        main = "Degree of Policy Nodes",
        xlab = "Degree",
        ylab = "",
        las = 1,
        xlim = c(0, max(deg_p_sorted) + 5),
        cex.names = 1.2,    # increase size of category labels (policy names)
        cex.axis = 1.2,     # increase tick label size (numbers)
        cex.lab  = 1.4      # increase x-axis label size)
        )
dev.off()


policy_nodes <- V(g_ip_pp)[type == TRUE]

SD_pci <- sapply(policy_nodes, function(p) {
  p_nodes <- neighbors(g_ip_pp, p)
  vals <- V(g_ip_pp)[p_nodes]$pci
  if (length(vals) < 2) return(NA)  # SD undefined for single product
  sd(vals, na.rm = TRUE)
})

valid_idx <- !is.na(SD_pci)

deg_valid <- deg_p[valid_idx]
SD_valid  <- SD_pci[valid_idx]

r <- cor(deg_valid, SD_valid)

png("Correlation_plot_product_sd.png", width = 1500, height = 800)

par(mar = c(6, 6, 4, 2))  # more space

plot(deg_valid, SD_valid,
     xlab = "Degree of Policy",
     ylab = "SD of PCI (Products)",
     pch = 19,
     cex.axis = 1.8,
     cex.lab  = 1.8)

abline(lm(SD_valid ~ deg_valid), col = "red", lwd = 3)

# correlation label
legend("topright",
       legend = paste0("r = ", round(r, 2)),
       bty = "n",
       cex = 2)

dev.off()

deg_p <- degree(g_ip_pp, v = V(g_ip_pp)[type == FALSE])

#Plotting Correlation of product's degree with its PCI
png("Correlation_plot_Product PCI.png", width = 1500, height = 800)

par(mar = c(5, 10, 5, 2))

plot(deg_p, V(g_ip_pp)[type == FALSE]$pci,
     xlab = "Degree of product",
     ylab = "PCI",
     #cex.names = 1.2,    # increase size of category labels (policy names)
     cex.axis = 1.8,     # increase tick label size (numbers)
     cex.lab  = 1.8,      # increase x-axis label size)
     pch = 19)

# Add regression line
abline(lm(V(g_ip_pp)[type == FALSE]$pci ~ deg_p), col = "red", lwd = 2)

r <- cor(deg_p, V(g_ip_pp)[type == FALSE]$pci)
  legend("topright", legend = paste0("r = ", round(r, 2)), bty = "n",cex = 2)

dev.off()

#Analysis for Objective 1
library(igraph)
library(dplyr)

countries <- unique(df_ip_cp$country)
policies  <- unique(c(df_ip_cp$policy, df_ip_pp$policy))
products  <- unique(df_ip_pp$product)

edges_ip <- rbind(
  df_ip_cp %>% rename(from = country, to = policy),
  df_ip_pp %>% rename(from = policy, to = product)
)
g_ip <- graph_from_data_frame(d = edges_ip, directed = FALSE)

V(g_ip)$type <- case_when(
  V(g_ip)$name %in% countries ~ "country",
  V(g_ip)$name %in% policies  ~ "policy",
  V(g_ip)$name %in% products  ~ "product",
  TRUE ~ "unknown"
)

countries <- unique(df_nonip_cp$country)
policies  <- unique(c(df_nonip_cp$policy, df_nonip_pp$policy))
products  <- unique(df_nonip_pp$product)

edges_nonip <- rbind(
  df_nonip_cp %>% rename(from = country, to = policy),
  df_nonip_pp %>% rename(from = policy, to = product)
)
g_nonip <- graph_from_data_frame(d = edges_nonip, directed = FALSE)


V(g_nonip)$type <- case_when(
  V(g_nonip)$name %in% countries ~ "country",
  V(g_nonip)$name %in% policies  ~ "policy",
  V(g_nonip)$name %in% products  ~ "product",
  TRUE ~ "unknown"
)

g_ip

g_nonip

summarize_tripartite <- function(g) {
  total_nodes <- vcount(g)
  total_edges <- ecount(g)
  types <- table(V(g)$type)      # adjust if role attribute used
  dens <- edge_density(g, loops = FALSE)
  list(nodes=total_nodes, edges=total_edges, density = dens, type_counts = types)
}

summ1 <- summarize_tripartite(g_ip)
summ2 <- summarize_tripartite(g_nonip)

vertex_attr_names(g_ip)

summ1

summ2

tripartite_density <- function(g) {
  # Count nodes by type
  nC <- sum(V(g)$type == "country")
  nP <- sum(V(g)$type == "policy")
  nPr <- sum(V(g)$type == "product")
  
  # Actual edges
  E_actual <- ecount(g)
  
  # Maximum possible edges
  E_max <- (nC * nP) + (nP * nPr)
  
  # Density
  density <- E_actual / E_max
  return(density)
}

dens_ip <- tripartite_density(g_ip)
dens_nonip <- tripartite_density(g_nonip)

dens_ip
dens_nonip


library(tidygraph)
library(ggraph)
#Plotting

print(1)
#Defining node types

V(g_ip)$type <- ifelse(V(g_ip)$name %in% df_ip_cp$country, "Country",
                  ifelse(V(g_ip)$name %in% df_ip_cp$policy, "Policy", "Product"))

tg_IP <- as_tbl_graph(g_ip)

V(g_nonip)$type <- ifelse(V(g_nonip)$name %in% df_nonip_cp$country, "Country",
                  ifelse(V(g_nonip)$name %in% df_nonip_cp$policy, "Policy", "Product"))

tg_nonIP <- as_tbl_graph(g_nonip)

print(2)

# Assigning coordinates
layout_ip <- tg_IP %>%
  activate(nodes) %>%
  as_tibble() %>%
  group_by(type) %>%
  mutate(
    x = scales::rescale(row_number(), to = c(0, 1)),
    y = case_when(
      type == "Country" ~ 3,
      type == "Policy"  ~ 2,
      type == "Product" ~ 1
    )
  ) %>%
  ungroup()


layout_nonip <- tg_nonIP %>%
  activate(nodes) %>%
  as_tibble() %>%
  group_by(type) %>%
  mutate(
    x = scales::rescale(row_number(), to = c(0, 1)),
    y = case_when(
      type == "Country" ~ 3,
      type == "Policy"  ~ 2,
      type == "Product" ~ 1
    )
  ) %>%
  ungroup()


print(3)

manual_layout_ip <- create_layout(graph = tg_IP, layout = layout_ip)

manual_layout_nonip <- create_layout(graph = tg_nonIP, layout = layout_nonip)

print(4)

p_ip <- ggraph(manual_layout_ip) +
  geom_edge_link(alpha = 0.2, color = "gray50") +
  geom_node_point(aes(color = type), size = 3) +
  scale_color_manual(values = c(
    Country = "steelblue",
    Policy  = "orange",
    Product = "seagreen"
  )) +
  theme_void() +
  labs(title = "Tripartite Industrial Policy Network") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

print(p_ip)

p_nonip <- ggraph(manual_layout_nonip) +
  geom_edge_link(alpha = 0.2, color = "gray50") +
  geom_node_point(aes(color = type), size = 3) +
  scale_color_manual(values = c(
    Country = "steelblue",
    Policy  = "orange",
    Product = "seagreen"
  )) +
  theme_void() +
  labs(title = "Tripartite Non-Industrial Policy Network") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

print(p_nonip)

print(5)

ggsave("/kaggle/working/Tripartite_IP.png", plot = p_ip, width = 14, height = 8, dpi = 300)

ggsave("/kaggle/working/Tripartite_nonIP.png", plot = p_nonip, width = 14, height = 8, dpi = 300)

print(6)

get_deg_by_type <- function(g) {
  types <- unique(V(g)$type)
  out <- list()
  for(t in types) {
    nodes <- V(g)[type == t]
    out[[as.character(t)]] <- degree(g, v = nodes)
  }
  out
}

deg1 <- get_deg_by_type(g_ip)
deg2 <- get_deg_by_type(g_nonip)

# Compare visually and with KS test for each type
png("Degree_Distribution.png", width = 1000, height = 500)
par(mfrow=c(3,2))
i <- 1
for(t in names(deg1)) {
  hist(deg1[[t]], main=paste("IP - type",t), xlab="degree")
  hist(deg2[[t]], main=paste("Non IP deg - type",t), xlab="degree")
  cat("KS test for type", t, ":", ks.test(deg1[[t]], deg2[[t]])$p.value, "\n")
  i <- i+1
}
dev.off()

library(igraph)
library(dplyr)

nodes_ip <- data.frame(
  name = c(unique(df_ip_pp$policy), unique(df_ip_pp$product)),
  type = c(rep(FALSE, length(unique(df_ip_pp$policy))),
           rep(TRUE,  length(unique(df_ip_pp$product))))
)

edges_ip <- df_ip_pp %>%
  rename(from = policy, to = product)

g_ip_pp <- graph_from_data_frame(d = edges_ip, vertices = nodes_ip, directed = FALSE)


nodes_nonip <- data.frame(
  name = c(unique(df_nonip_pp$policy), unique(df_nonip_pp$product)),
  type = c(rep(FALSE, length(unique(df_nonip_pp$policy))),
           rep(TRUE,  length(unique(df_nonip_pp$product))))
)

edges_nonip <- df_nonip_pp %>%
  rename(from = policy, to = product)

g_nonip_pp <- graph_from_data_frame(d = edges_nonip, vertices = nodes_nonip, directed = FALSE)

#Identify Product Nodes
product_nodes_ip    <- V(g_ip_pp)[type == TRUE]
product_nodes_nonip <- V(g_nonip_pp)[type == TRUE]

#Jaccard Similarity
jac_ip  <- similarity(g_ip_pp, vids = product_nodes_ip, method = "jaccard")
jac_non <- similarity(g_nonip_pp, vids = product_nodes_nonip, method = "jaccard")

#Upper triangle
sim_ip    <- jac_ip[upper.tri(jac_ip)]
sim_nonip <- jac_non[upper.tri(jac_non)]

#Similarity Distribution's statistical test
ks.test(sim_ip, sim_nonip)

# Boxplot
png("Box_Plot.png", width = 1000, height = 500)
boxplot(sim_ip, sim_nonip,
        names = c("IP", "Non-IP"),
        main = "Product–Product Jaccard Similarity",
        ylab = "Jaccard similarity")
dev.off()
# Summary statistics
summary(sim_ip)
summary(sim_nonip)
var(sim_ip)
var(sim_nonip)
