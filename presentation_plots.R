sum(!complete.cases(study2_clean))
# 115 rows with at least one missing value 

describe(study2_clean$p_age)

table(study2_clean$p_race)

ggplot(mca_pred_esteem,
       aes(x = p_gender, fill = p_gender)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Participant Gender') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'gender_bar.tiff', dpi = 600)

ggplot(mca_pred_esteem,
       aes(x = IMS_total_bin, fill = IMS_total_bin)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Relationship Quality') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'RQ_bar.tiff', dpi = 600)


ggplot(mca_pred_esteem, aes(x = esteem_avg)) +
  geom_histogram(binwidth = .5, fill = '#C8350DFF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(title = 'Self-Esteem of Sample',
       x = 'Self-Esteem Score') 
ggsave(filename = 'self_esteem_bar.tiff', dpi = 600)

describe(mca_pred_esteem$esteem_avg)

ggplot(mca_pred_esteem, aes(x = dim.1)) +
  geom_histogram(binwidth = .3, fill = '#81B28DFF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = 'Dimension 1 Scores',
       title = 'Dimension 1')
ggsave(filename = 'dim1_hist.tiff', dpi = 600)


ggplot(mca_pred_esteem, aes(x = dim.2)) +
  geom_histogram(binwidth = .2, fill = '#81B28DFF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = 'Dimension 2 Scores',
       title = 'Dimension 2')
ggsave(filename = 'dim2_hist.tiff', dpi = 600)

ggplot(mca_pred_esteem, aes(x = dim.3)) +
  geom_histogram(binwidth = .1, fill = '#81B28DFF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = 'Dimension 3 Scores',
       title = 'Dimension 3')
ggsave(filename = 'dim3_hist.tiff', dpi = 600)

ggplot(mca_pred_esteem, aes(x = dim.4)) +
  geom_histogram(binwidth = .2, fill = '#81B28DFF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = 'Dimension 4 Scores',
       title = 'Dimension 4')
ggsave(filename = 'dim4_hist.tiff', dpi = 600)


fviz_mca_ind(mca_model.3,
             label = 'none',
             habillage = 'rel_understanding_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2),
             pointsize = 2.5) +
  scale_color_paletteer_d('MoMAColors::Klein') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 26),
        plot.title = element_text(size = 30),
        legend.position = 'none') +
  labs(title = 'Relative Understanding')
ggsave(filename = 'rel_understand_dim_1_2.tiff', dpi = 600)

fviz_mca_ind(mca_model.3,
             label = 'none',
             habillage = 'IMS_total_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2),
             pointsize = 2.5) +
  scale_color_paletteer_d('MoMAColors::Klein') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.position = 'none') +
  labs(title = 'Relationship Quality')
ggsave(filename = 'rel_qual_dim_1_2.tiff', dpi = 600)
