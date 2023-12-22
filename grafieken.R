# Laad de benodigde bibliotheken
library(ggplot2)
library(dplyr)

# Lees de data_melihset in
data_melih <- Dataset.OIS...Dataset

# Bereken de verschillen in hartslag
data_melih$Diff.Gem.HR <- data_melih$`Gem.HR.T` - data_melih$`Gem.HR.B`
data_melih$Diff.Max.HR <- data_melih$`Max.HR.T` - data_melih$`Max.HR.B`
data_melih$Diff.Min.HR <- data_melih$`Min.HR.T` - data_melih$`Min.HR.B`

#----------------------------------------------------------------------------------------------------------
# Grafiek voor Verschil in Gemiddelde Hartslag
ggplot(data_melih, aes(x = Type.test, y = Diff.Gem.HR, fill = Type.test)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Verschil in Gemiddelde Hartslag (Baseline vs Tijdsdruk)", y = "Gemiddeld Verschil in Hartslag")

#----------------------------------------------------------------------------------------------------------
# Grafiek voor Verschil in Max en Min Hartslag
plot_data_melih <- data.frame(
  Type.test = rep(data_melih$Type.test, 2),
  Hartslag.Type = rep(c("Max", "Min"), each = nrow(data_melih)),
  Hartslag.Verschil = c(data_melih$Diff.Max.HR, data_melih$Diff.Min.HR)
)

# Creëer een interactie tussen Type.test en Hartslag.Type
plot_data_melih$Interaction <- with(plot_data_melih, interaction(Type.test, Hartslag.Type))

# Maak de grafiek
ggplot(plot_data_melih, aes(x = Type.test, y = Hartslag.Verschil, fill = Interaction)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  scale_fill_manual(values = c("Laptop.Max" = "darkred", 
                               "Laptop.Min" = "tomato", 
                               "VR.Max" = "darkblue", 
                               "VR.Min" = "lightblue")) +
  labs(title = "Verschil in Max en Min Hartslag tussen Baseline en Tijdsdruk", 
       x = "Test Type", 
       y = "Verschil in Hartslag (T - B)")
#----------------------------------------------------------------------------------------------------------
# Grafiek voor Hoeveelheid Fouten
ggplot(data_melih, aes(x = Type.test, y = `Fouten.T`, fill = Type.test)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Hoeveelheid Fouten (Tijdsdruk)", y = "Gemiddeld Aantal Fouten")

#----------------------------------------------------------------------------------------------------------
# Grafiek met Deelnemers met Hoge Hartslag
high.hr.threshold <- quantile(data_melih$`Gem.HR.B`, 0.75)
high.hr.data_melih <- data_melih[data_melih$`Gem.HR.B` > high.hr.threshold, ]
ggplot(high.hr.data_melih, aes(x = as.factor(Participant), y = `Gem.HR.B`, fill = Type.test)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Deelnemers met Hoge Hartslag (Baseline)", y = "Hartslag") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Om de x-as labels verticaal te maken

#----------------------------------------------------------------------------------------------------------
# Bereken het verschil in fouten tussen Tijdsdruk en Baseline
data_melih$Verschil.Fouten <- data_melih$Fouten.T - data_melih$Fouten.B

# Maak de boxplot
ggplot(data_melih, aes(x = Type.test, y = Verschil.Fouten, fill = Geslacht)) +
  geom_boxplot(outlier.shape = NA) + # Om outliers niet te tonen
  scale_fill_manual(values = c("Man" = "lightblue", "Vrouw" = "pink")) +
  labs(title = "Verschil in Fouten tussen Baseline en Tijdsdruk per Geslacht en Type Test",
       x = "Type Test",
       y = "Verschil in Fouten (Tijdsdruk - Baseline)") +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------
# Regression coefficients and standard errors for Tijdsduur.B
coefficients_tijdsduur <- c(4.728, 1.003, 2.509, 78.126)
std_errors_tijdsduur <- c(3.694, 1.003, 3.719, 21.315)
variables_tijdsduur <- c("Type.testVR", "Leeftijd", "GeslachtVrouw", "Intercept")

# Regression coefficients and standard errors for Fouten.B
coefficients_fouten <- c(-0.12345, 0.01707, -0.08894, 0.85457)
std_errors_fouten <- c(0.29850, 0.08101, 0.30046, 1.72221)
variables_fouten <- c("Type.testVR", "Leeftijd", "GeslachtVrouw", "Intercept")

# Function to create a plot for regression coefficients
plot_coefficients <- function(coefficients, std_errors, variables, title) {
  df <- data.frame(Variable = variables, Coefficient = coefficients, SE = std_errors)
  df$CI_Lower <- df$Coefficient - 1.96 * df$SE
  df$CI_Upper <- df$Coefficient + 1.96 * df$SE
  
  ggplot(df, aes(x = Variable, y = Coefficient)) +
    geom_point() +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = title, x = "Variabele", y = "Geschatte Coëfficiënt") +
    theme_minimal()
}

# Plot for Tijdsduur.B
plot_coefficients(coefficients_tijdsduur, std_errors_tijdsduur, variables_tijdsduur, 
                  "Regressiecoëfficiënten voor Tijdsduur.B")

# Plot for Fouten.B
plot_coefficients(coefficients_fouten, std_errors_fouten, variables_fouten, 
                  "Regressiecoëfficiënten voor Fouten.B")

#----------------------------------------------------------------------------------------------------------
# Create a new data_melih frame for plotting
data_melih_long <- data.frame(
  Participant = rep(data_melih$Participant, 4),
  Type_test = rep(data_melih$Type.test, 4),
  Condition = rep(c("Tijdsduur.B", "Tijdsduur.T", "Fouten.B", "Fouten.T"), each = nrow(data_melih)),
  Value = c(data_melih$Tijdsduur.B, data_melih$Tijdsduur.T, data_melih$Fouten.B, data_melih$Fouten.T)
)

# Boxplots for Time Duration and Number of Errors
ggplot(data_melih_long[data_melih_long$Condition %in% c("Tijdsduur.B", "Tijdsduur.T"),], 
       aes(x = Condition, y = Value, fill = Type_test)) + 
  geom_boxplot() +
  facet_wrap(~Type_test) +
  labs(title = "Tijdsduur: Baseline vs Tijdsdruk", x = "Conditie", y = "Tijdsduur") +
  theme_minimal()

ggplot(data_melih_long[data_melih_long$Condition %in% c("Fouten.B", "Fouten.T"),], 
       aes(x = Condition, y = Value, fill = Type_test)) + 
  geom_boxplot() +
  facet_wrap(~Type_test) +
  labs(title = "Aantal Fouten: Baseline vs Tijdsdruk", x = "Conditie", y = "Aantal Fouten") +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------
# Scatterplot for VR Group
data_melih_vr <- data_melih[data_melih$Type.test == 'VR',]
ggplot(data_melih_vr, aes(x = Tijdsduur.T, y = Fouten.T)) +
  geom_point(shape = 19, size = 3, color = "darkorange") +  # Adjust point shape, size, and color
  geom_smooth(method = 'lm', se = FALSE, color = 'dodgerblue', size = 1.2) +  # Improve line aesthetics
  labs(
    title = "Correlatie tussen Tijdsduur en Fouten in VR-Groep",
    x = "Tijdsduur (seconden)",
    y = "Aantal Fouten"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Enhance title
    axis.title = element_text(size = 12, face = "bold"),  # Enhance axis labels
    axis.text = element_text(size = 10)  # Adjust axis text size
  )

# Scatterplot for Laptop Group
data_melih_laptop <- data_melih[data_melih$Type.test == 'Laptop',]
ggplot(data_melih_laptop, aes(x = Tijdsduur.T, y = Fouten.T)) +
  geom_point(shape = 19, size = 3, color = "darkorange") +  # Adjust point shape, size, and color
  geom_smooth(method = 'lm', se = FALSE, color = 'dodgerblue', size = 1.2) +  # Improve line aesthetics
  labs(
    title = "Correlatie tussen Tijdsduur en Fouten in Laptopgroep",
    x = "Tijdsduur (seconden)",
    y = "Aantal Fouten"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Enhance title
    axis.title = element_text(size = 12, face = "bold"),  # Enhance axis labels
    axis.text = element_text(size = 10)  # Adjust axis text size
  )

#----------------------------------------------------------------------------------------------------------
# Creating a new data_melih frame for plotting
data_melih_long <- data.frame(
  Participant = rep(data_melih$Participant, 4),
  Type_test = rep(data_melih$Type.test, 4),
  Condition = rep(c("Tijdsduur.B", "Tijdsduur.T", "Fouten.B", "Fouten.T"), each = nrow(data_melih)),
  Value = c(data_melih$Tijdsduur.B, data_melih$Tijdsduur.T, data_melih$Fouten.B, data_melih$Fouten.T)
)

# Point Plots for Time Duration
ggplot(data_melih_long[data_melih_long$Condition %in% c("Tijdsduur.B", "Tijdsduur.T"),], 
       aes(x = Condition, y = Value, group = Participant)) +
  geom_point(aes(color = Type_test)) +
  geom_line(aes(color = Type_test)) +
  facet_wrap(~Type_test) +
  labs(title = "Individuele Veranderingen in Tijdsduur", x = "Conditie", y = "Tijdsduur") +
  theme_minimal()

# Point Plots for Number of Errors
ggplot(data_melih_long[data_melih_long$Condition %in% c("Fouten.B", "Fouten.T"),], 
       aes(x = Condition, y = Value, group = Participant)) +
  geom_point(aes(color = Type_test)) +
  geom_line(aes(color = Type_test)) +
  facet_wrap(~Type_test) +
  labs(title = "Individuele Veranderingen in Aantal Fouten", x = "Conditie", y = "Aantal Fouten") +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------
# Calculate the differences and add as new columns
data_melih <- data_melih %>%
  mutate(Tijdsduur.Difference = Tijdsduur.T - Tijdsduur.B,
         Fouten.Difference = Fouten.T - Fouten.B)

# Now you can proceed with the ggplot2 code to create the plot
ggplot(data_melih, aes(x = Tijdsduur.Difference, y = Fouten.Difference)) +
  geom_density_2d_filled() +
  facet_wrap(~ Type.test) + # Splits de plot op basis van de testtype
  labs(title = "Bi-variate Distribution: Tijdsduur Verschil vs. Fouten Verschil",
       x = "Tijdsduur Verschil (T - B)",
       y = "Fouten Verschil (T - B)") +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------
# Bereken eerst het verschil in gemiddelde hartslag
data_melih$HR_Difference = data_melih$Gem.HR.T - data_melih$Gem.HR.B

# Orden de data_melih eerst op basis van Type.test en dan op Participant
data_melih <- data_melih %>% 
  arrange(Type.test, Participant)

# Maak een nieuwe factor variabele voor de geordende Participant
data_melih$Ordered_Participant <- factor(data_melih$Participant, levels = unique(data_melih$Participant))

# Maak de scatterplot
ggplot(data_melih, aes(x = Ordered_Participant, y = HR_Difference, color = Type.test)) +
  geom_point(size = 3) +
  geom_segment(aes(xend = Ordered_Participant, yend = 0), linetype = "dashed") +
  geom_hline(yintercept = 0, size = 1.2, color = "black") +
  theme_bw() +
  labs(title = "Verschil Gemiddelde Hartslag Tussen Baseline en Tijdsdruk",
       x = "",
       y = "Verschil in Gemiddelde Hartslag (T-B)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid.major.y = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank(),  
        panel.grid.minor.y = element_line(color = "gray", size = 0.2),
        legend.position = "bottom",
        axis.line.x = element_blank(),  #
        axis.line.y = element_line(color = "black", size = 1.2)) +  
  scale_y_continuous(breaks = seq(min(data_melih$HR_Difference), max(data_melih$HR_Difference), by = 2)) +
  scale_color_manual(values = c("VR" = "red", "Laptop" = "steelblue"))
#----------------------------------------------------------------------------------------------------------
ggplot(data_melih, aes(x = factor(1), y = GemHRDifference, fill = Type.test)) +
  geom_violin(alpha = 0.7, position = position_dodge(width = 0.0), width = 1) +
  scale_fill_manual(values = c("Laptop" = "steelblue", "VR" = "red")) +
  labs(
    title = "Vergelijking van Gemiddelde Hartslag (HR in tijdsdruktest - HR in baseline) tussen Laptop en VR",
    x = "",
    y = "Hartslagverschil (BPM)"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", colour = "black"),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.title.x = element_blank()  
  ) +
  guides(fill = guide_legend(title = "Test Type")) +
  scale_y_continuous(breaks = seq(from = min(data_melih$GemHRDifference, na.rm = TRUE), to = max(data_melih$GemHRDifference, na.rm = TRUE), by = 3))
#----------------------------------------------------------------------------------------------------------
