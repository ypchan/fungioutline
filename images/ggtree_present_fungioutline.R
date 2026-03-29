library(fungioutline)
library(tidyverse)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(treemapify)

install.packages('treemapify')
# 假设你的数据框叫 outline
# str(outline) 里已有这些列：
# Kingdom, Subkingdom, Phylum, Subphylum, Class, Subclass, Order, Family, Genus

# =========================
# 1. 数据预处理
# =========================
dat <- outline %>%
    mutate(across(c(Phylum, Class, Order, Family, Genus), ~replace_na(.x, "Unknown"))) %>%
    mutate(across(c(Phylum, Class, Order, Family, Genus), ~ifelse(.x == "" | .x == "NA", "Unknown", .x)))

# =========================
# 2. 只保留前10个 Phylum，其余合并为 Other
# =========================
top_phyla <- dat %>%
    count(Phylum, sort = TRUE) %>%
    slice_head(n = 10) %>%
    pull(Phylum)

dat_sel <- dat %>%
    mutate(
        Phylum = ifelse(Phylum %in% top_phyla, Phylum, "Other")
    )

# =========================
# 3. 聚合到 Family 层级
#    每条记录视为一个 genus 记录
# =========================
plot_df <- dat_sel %>%
    count(Phylum, Class, Order, Family, name = "n") %>%
    arrange(desc(n))

# =========================
# 4. 为了避免标签太多，只给较大的块加标签
# =========================
plot_df <- plot_df %>%
    mutate(
        label_family = ifelse(n >= quantile(n, 0.90), Family, ""),
        label_order  = ifelse(n >= quantile(n, 0.97), Order, "")
    )

# =========================
# 5. 绘图：层级 treemap
# =========================
p <- ggplot(
    plot_df,
    aes(
        area = n,
        fill = Phylum,
        subgroup = Class,
        subgroups = Order,
        subgroup2 = Family
    )
) +
    geom_treemap(color = "white", linewidth = 0.6) +
    geom_treemap_subgroup_border(colour = "white", linewidth = 1.2) +
    geom_treemap_subgroup2_border(colour = "grey90", linewidth = 0.8) +
    geom_treemap_text(
        aes(label = label_family),
        place = "centre",
        grow = TRUE,
        reflow = TRUE,
        colour = "black",
        min.size = 3
    ) +
    geom_treemap_subgroup_text(
        aes(label = Class),
        place = "topleft",
        grow = FALSE,
        alpha = 0.9,
        colour = "black",
        fontface = "bold",
        min.size = 6
    ) +
    labs(
        title = "Fungal Outline (selective display)",
        subtitle = "Top 10 phyla shown in detail; remaining phyla grouped as 'Other'",
        fill = "Phylum"
    ) +
    theme_minimal(base_size = 10) +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 8)
    )

p
