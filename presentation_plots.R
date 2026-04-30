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

ggplot(mca_pred_esteem,
       aes(x = rel_understanding_bin, fill = rel_understanding_bin)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Relative Understanding') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'rel_understand_bin_bar.tiff', dpi = 600)

ggplot(mca_pred_esteem,
       aes(x = SWLS_bin, fill = SWLS_bin)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Subjective Well-being') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'swls_bin_bar.tiff', dpi = 600)

ggplot(mca_pred_esteem,
       aes(x = positive_affect_bin, fill = positive_affect_bin)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Positive Affect') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'pos_aff_bar.tiff', dpi = 600)

ggplot(mca_pred_esteem,
       aes(x = negative_affect_bin, fill = negative_affect_bin)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Negative Affect') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'neg_aff_bin_bar.tiff', dpi = 600)

ggplot(mca_pred_esteem,
       aes(x = SMS_visibility_bin, fill = SMS_visibility_bin)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Online Relationship Visibility') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'sms_vis_bar.tiff', dpi = 600)

ggplot(mca_pred_esteem,
       aes(x = p_trans, fill = p_trans)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26)) +
  labs(x = '',
       title = 'Transgender Identity') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'trans_bar.tiff', dpi = 600)

ggplot(mca_pred_esteem,
       aes(x = p_sexual_orientation, fill = p_sexual_orientation)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        legend.position = 'none',
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '',
       title = 'Sexual Orientation') +
  scale_fill_paletteer_d('MoMAColors::Klein')
ggsave(filename = 'sexual_orientation_bar.tiff', dpi = 600)


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

fviz_contrib(mca_model.3, 
             choice = 'var', 
             top = 20, 
             axes = c(2), 
             fill = '#2D2651FF',
             color = 'black') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 26),
        plot.margin = unit(c(0,0,0,2), 'cm')) +
  labs(title = '')
ggsave(filename = 'dim1_important.tiff', dpi = 600)

fviz_mca_var(mca_model.3, choice = 'var', axes = c(1,2),
             font.family = 'serif', col.var = '#FF4D6FFF', col.quanti.sup = '#579EA4FF',
             repel = TRUE,
             labelsize = 10,
             pointsize = 5) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 24),
        plot.margin = unit(c(0,0,0,0), 'cm')) +
  labs(title = '')
ggsave(filename = 'importance_dim_1_2.tiff', dpi = 600)

fviz_contrib(mca_model.3, 
             choice = 'var', 
             top = 20, 
             axes = c(1), 
             fill = '#2D2651FF',
             color = 'black') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 26),
        plot.margin = unit(c(0,0,0,2), 'cm')) +
  labs(title = '')
ggsave(filename = 'dim1_important.tiff', dpi = 600)


fviz_mca_var(mca_model.3, choice = 'var', axes = c(3,4),
             font.family = 'serif', col.var = '#FF4D6FFF', col.quanti.sup = '#579EA4FF',
             repel = TRUE,
             labelsize = 10,
             pointsize = 5) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 26),
        plot.margin = unit(c(0,0,0,0), 'cm')) +
  labs(title = '')
ggsave(filename = 'dim_3_4_important.tiff', dpi = 600)

fviz_contrib(mca_model.3, 
             choice = 'var', 
             top = 20, 
             axes = c(3), 
             fill = '#2D2651FF',
             color = 'black') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 26)) +
  labs(title = '')
ggsave(filename = 'dim3_important.tiff', dpi = 600)


fviz_contrib(mca_model.3, 
             choice = 'var', 
             top = 20, 
             axes = c(4), 
             fill = '#2D2651FF',
             color = 'black') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 26),
        plot.margin = unit(c(0, 0, 0, 1), 'cm')) +
  labs(title = '')
ggsave(filename = 'dim4_important.tiff', dpi = 600)

fviz_mca_ind(mca_model.3,
             label = 'none',
             habillage = 'p_trans',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(3,4),
             pointsize = 2.5) +
  scale_color_paletteer_d('MoMAColors::Klein') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 26),
        legend.position = 'none') +
  labs(title = 'Transgender Identity')
ggsave(filename = 'trans_dim_3_4.tiff', dpi = 600)


fviz_mca_ind(mca_model.3,
             label = 'none',
             habillage = 'p_gender',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(3,4),
             pointsize = 2.5) +
  scale_color_paletteer_d('MoMAColors::Klein') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 26),
        legend.position = 'none') +
  labs(title = 'Participant Gender')
ggsave(filename = 'gender_dims_3_4.tiff', dpi = 600)


fviz_mca_ind(mca_model.3,
             label = 'none',
             habillage = 'rel_distance',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(3,4),
             pointsize = 2.5) +
  scale_color_paletteer_d('MoMAColors::Klein') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 26),
        legend.position = 'none') +
  labs(title = 'Relationship Long-Distance')
ggsave(filename = 'rel_distance_dims_3_4.tiff', dpi = 600)

fviz_screeplot(mca_model.3,
               barfill = '#7E1A2FFF',
               barcolor = 'black') +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 26)) +
  labs(title = '')
ggsave(filename = 'scree_plot.tiff', dpi = 600)


ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg)) +
  geom_density(aes(fill = IMS_total_bin), alpha = .7) +
  geom_vline(aes(xintercept = mean(esteem_avg)), linetype = 'dashed') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 26)) +
  labs(x = 'Self-Esteem',
       y = 'Density',
       title = 'Relationship Quality') +
  scale_fill_paletteer_d('MoMAColors::Klein', name = 'Relationship Quality', label = c('Low','Mid','High'))
ggsave(filename = 'density_RQ.tiff', dpi = 600)


ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg)) +
  geom_density(aes(fill = rel_understanding_bin), alpha = .7) +
  geom_vline(aes(xintercept = mean(esteem_avg)), linetype = 'dashed') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 26)) +
  labs(x = 'Self-Esteem',
       y = 'Density',
       title = 'Relative Understanding') +
  scale_fill_paletteer_d('MoMAColors::Klein', name = 'Relationship Quality', label = c('Low','Mid','High'))
ggsave(filename = 'rel_understand_density.tiff', dpi = 600)


ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg)) +
  geom_density(aes(fill = p_trans), alpha = .7) +
  geom_vline(aes(xintercept = mean(esteem_avg)), linetype = 'dashed') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 26)) +
  labs(x = 'Self-Esteem',
       y = 'Density',
       title = 'Transgender Identity') +
  scale_fill_paletteer_d('MoMAColors::Klein', name = 'Identity', label = c('Transgender', 'Cisgender'))
ggsave(filename = 'trans_density.tiff', dpi = 600)

ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg)) +
  geom_density(aes(fill = p_gender), alpha = .7) +
  geom_vline(aes(xintercept = mean(esteem_avg)), linetype = 'dashed') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 26)) +
  labs(x = 'Self-Esteem',
       y = 'Density',
       title = 'Gender') +
  scale_fill_paletteer_d('MoMAColors::Klein', name = 'Gender')
ggsave(filename = 'gender_density.tiff', dpi = 600)


ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg, y = dim.1)) +
  geom_point(color = '#C8350DFF') +
  geom_smooth(method = 'lm', color = '#579EA4FF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 26),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  labs(title = 'Self-Esteem and Dimension 1',
       x = 'Self-Esteem',
       y = 'Dimension 1')
ggsave(filename = 'esteem_dim1_scat.tiff', dpi = 600)

ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg, y = dim.2)) +
  geom_point(color = '#C8350DFF') +
  geom_smooth(method = 'lm', color = '#579EA4FF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 26),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  labs(title = 'Self-Esteem and Dimension 2',
       x = 'Self-Esteem',
       y = 'Dimension 2')
ggsave(filename = 'esteem_dim2_scat.tiff', dpi = 600)

ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg, y = dim.3)) +
  geom_point(color = '#C8350DFF') +
  geom_smooth(method = 'lm', color = '#579EA4FF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 26),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  labs(title = 'Self-Esteem and Dimension 3',
       x = 'Self-Esteem',
       y = 'Dimension 3')
ggsave(filename = 'esteem_dim3_scat.tiff', dpi = 600)

ggplot(data = mca_pred_esteem,
       aes(x = esteem_avg, y = dim.4)) +
  geom_point(color = '#C8350DFF') +
  geom_smooth(method = 'lm', color = '#579EA4FF') +
  theme_classic() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 26),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  labs(title = 'Self-Esteem and Dimension 4',
       x = 'Self-Esteem',
       y = 'Dimension 4')
ggsave(filename = 'esteem_dim4_scat.tiff', dpi = 600)
