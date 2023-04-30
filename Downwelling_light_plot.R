library(tidyverse)

# Attenutation coefficient data for different waters (From "Marine Optics" Jerlov 1976)
Kd_df <- read.csv("kd_Jerlov1976.csv")

# Calculates a smooth curve of Kd values across wavelength using Loess smoothing
# Calculates the depth at which 1% of surface irradiance remains
Spectrum_depth <- data.frame(Wavelength = 350:700) %>%
	left_join(Kd_df) %>%
	gather('Water_type', 'Kd', -Wavelength) %>%
	mutate(Water_type = factor(Water_type, 
		levels = colnames(Kd_df[-1]))) %>%
  	group_by(Water_type) %>% 
	mutate(Kd_Loess = predict(loess(Kd ~ Wavelength),
  	data.frame(Wavelength = Wavelength))) %>%
  	mutate(Depth_plot = (log(100) / Kd_Loess))

# Identities the maximum depth reached by 1% of surface irradiance for each water type
Max_depth <- Spectrum_depth %>%
	group_by(Water_type) %>%
	slice(which.max(Depth_plot))

# Plots the curves showing where irradiance is at 1% surface levels 
# Maximum depth indicated by a circle
# Y-axis scaled by depth values for each water type
ggplot() + 
	geom_vline(xintercept = seq(400,700, by = 50), colour = 'grey90') +
	geom_smooth(data = Spectrum_depth, aes(Wavelength, Depth_plot * -1), 
		method = 'loess', span = 0.25, se=FALSE, colour = 'black') + 
	geom_point(data = Max_depth, aes(Wavelength, Depth_plot * -1),
		shape = 21, fill = 'black', colour = 'white', size = 3, stroke = 2) +
	theme_test() + 
	scale_x_continuous(name ='Wavelength (nm)') +
	scale_y_continuous(name ='Depth (m)') +
   	facet_grid(Water_type ~ ., scales='free_y')

# Saves the plot
ggsave('downwelling_light.pdf')
