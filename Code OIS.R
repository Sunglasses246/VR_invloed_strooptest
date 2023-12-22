library("ggplot2")
library("psych")
library("knitr")
library(dplyr)
ds <- `Dataset.OIS...Dataset`
data <- ds

# Verschillende databases voor VR en Laptop
data_vr <- data[data$Type.test == "VR",]
data_laptop <- data[data$Type.test == "Laptop",]

# Kolom voor verschillen tijdsduur tussen tijdsdruk en baseline test
data$Time_Difference <- data$Tijdsduur.T - data$Tijdsduur.B
data_vr$HR_AVG_difference <- data_vr$Gem.HR.T - data_vr$Gem.HR.B
data_laptop$HR_AVG_difference <- data_laptop$Gem.HR.T - data_laptop$Gem.HR.B

# Visualisatie van het tijdsverschil per deelnemer
ggplot(data, aes(x=factor(Participant), y=Time_Difference)) +
  geom_bar(stat="identity", fill="purple") +
  labs(title="Tijdsverschil tussen tijdsdruk test vs baseline test",
       x="Deelnemer",
       y="Tijdsverschil in Seconden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Laptop verschil
ggplot(data_laptop, aes(x=factor(Participant), y=Time_Difference)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Tijdsverschil tussen tijdsdruk test vs baseline test laptop",
       x="Deelnemer",
       y="Tijdsverschil in Seconden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#VR verschil
ggplot(data_vr, aes(x=factor(Participant), y=Time_Difference)) +
  geom_bar(stat="identity", fill="red") +
  labs(title="Tijdsverschil tussen tijdsdruk test vs baseline test vr",
       x="Deelnemer",
       y="Tijdsverschil in Seconden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Uitvoeren van de onafhankelijke t-toets
t.test(data_vr$Time_Difference, data_laptop$Time_Difference)

# Visualisatie van de gemiddelde hartslag voor VR en Laptop tests
ggplot(data, aes(x=Type.test, y=Gem.HR.B, fill=Type.test)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Laptop" = "steelblue", "VR" = "red")) +
  labs(title="Gemiddelde Hartslag tijdens Baseline Test voor VR en Laptop",
       x="Type Test",
       y="Gemiddelde Hartslag (Baseline)") +
  theme_minimal()

ggplot(data, aes(x=Type.test, y=Gem.HR.T, fill=Type.test)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Laptop" = "steelblue", "VR" = "red")) +
  labs(title="Gemiddelde Hartslag tijdens Tijdsdruk Test voor VR en Laptop",
       x="Type Test",
       y="Gemiddelde Hartslag (Tijdsdruk)") +
  theme_minimal()

#Hartslag verschil
#Laptop
ggplot(data_laptop, aes(x=factor(Participant), y=HR_AVG_difference)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Hartslagverschil tussen tijdsdruk test vs baseline test Laptop",
       x="Deelnemer",
       y="Tijdsverschil in Seconden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#VR
ggplot(data_vr, aes(x=factor(Participant), y=HR_AVG_difference)) +
  geom_bar(stat="identity", fill="red") +
  labs(title="Hartslagverschil tussen tijdsdruk test vs baseline test VR",
       x="Deelnemer",
       y="Tijdsverschil in Seconden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Visualisatie van de maximale voor VR en Laptop tests baseline
ggplot(data, aes(x=Type.test, y=Max.HR.B, fill=Type.test)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Laptop" = "steelblue", "VR" = "red")) +
  labs(title="Maximale Hartslag tijdens Baseline Test voor VR en Laptop",
       x="Type Test",
       y="Gemiddelde Hartslag (Baseline)") +
  theme_minimal()

# Visualisatie van de minimale hartslag voor VR en Laptop tests baseline
ggplot(data, aes(x=Type.test, y=Min.HR.B, fill=Type.test)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Laptop" = "steelblue", "VR" = "red")) +
  labs(title="Minimale hartslag tijdens Baseline Test voor VR en Laptop",
       x="Type Test",
       y="Gemiddelde Hartslag (Baseline)") +
  theme_minimal()

# Visualisatie van de maximale hartslag voor VR en Laptop tests tijdsdruj
ggplot(data, aes(x=Type.test, y=Min.HR.T, fill=Type.test)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Laptop" = "steelblue", "VR" = "red")) +
  labs(title="Maximale hartslag tijdens Tijdsdruk Test voor VR en Laptop",
       x="Type Test",
       y="Gemiddelde Hartslag (Baseline)") +
  theme_minimal()

# Visualisatie van de minimale hartslag voor VR en Laptop tests tijdsdruk
ggplot(data, aes(x=Type.test, y=Min.HR.T, fill=Type.test)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Laptop" = "steelblue", "VR" = "red")) +
  labs(title="Minimale hartslag tijdens Tijdsdruk Test voor VR en Laptop",
       x="Type Test",
       y="Gemiddelde Hartslag (Baseline)") +
  theme_minimal()


#Verschil minimale en maximale hartslag Laptop baseline

#Laptop
data_laptop$MIN_MAX_difference_B <- data_laptop$Max.HR.B - data_laptop$Min.HR.B

ggplot(data_laptop, aes(x=factor(Participant), y = MIN_MAX_difference_B)) +
  geom_bar(stat = "identity", fill="steelblue") +
  labs(title = "Verschil maximum en minimum hartslag participant bij baseline test laptop",
       x = "Participant",
       y = "Verschil Hartslag") +
  theme_minimal()

#VR
data_vr$MIN_MAX_difference_B <- data_vr$Max.HR.B - data_vr$Min.HR.B

ggplot(data_vr, aes(x=factor(Participant), y = MIN_MAX_difference_B)) +
  geom_bar(stat = "identity", fill="red") +
  labs(title = "Verschil maximum en minimum hartslag participant bij baseline test VR",
       x = "Participant",
       y = "Verschil Hartslag") +
  theme_minimal()

#Verschil minimale en maximale hartslag Laptop tijdsdruk

#Laptop
data_laptop$MIN_MAX_difference_T <- data_laptop$Max.HR.T - data_laptop$Min.HR.T

ggplot(data_laptop, aes(x=factor(Participant), y = MIN_MAX_difference_T)) +
  geom_bar(stat = "identity", fill="steelblue") +
  labs(title = "Verschil maximum en minimum hartslag participant bij tijdsdruk test laptop",
       x = "Participant",
       y = "Verschil Hartslag") +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 35))

#VR
data_vr$MIN_MAX_difference_T <- data_vr$Max.HR.T - data_vr$Min.HR.T

ggplot(data_vr, aes(x=factor(Participant), y = MIN_MAX_difference_T)) +
  geom_bar(stat = "identity", fill="red") +
  labs(title = "Verschil maximum en minimum hartslag participant bij tijdsdruk test VR",
       x = "Participant",
       y = "Verschil Hartslag") +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 35))

#T-test laptop & VR
data_vr$MIS_DIFF <- data_vr$Fouten.T - data_vr$Fouten.B
data_laptop$MIS_DIFF <- data_laptop$Fouten.T - data_laptop$Fouten.B
t.test(data_laptop$Gem.HR.T, data_vr$Gem.HR.T)
t.test(data_laptop$Min.HR.T, data_vr$Min.HR.T)
t.test(data_laptop$Max.HR.T, data_vr$Max.HR.T)
t.test(data_laptop$MIN_MAX_difference_T, data_vr$MIN_MAX_difference_T)
t.test(data_vr$Gem.HR.B, data_vr$Gem.HR.T, paired = TRUE)
t.test(data_laptop$Gem.HR.B, data_laptop$Gem.HR.T, paired = TRUE)
t.test(data_vr$Fouten.T, data_vr$Fouten.B, paired = TRUE)
t.test(data_laptop$Fouten.T, data_laptop$Fouten.B, paired = TRUE)
t.test(data_vr$Tijdsduur.T, data_laptop$Tijdsduur.T)
t.test(data_vr$Fouten.T, data_laptop$Fouten.T)
t.test(data_vr$Gem.HR.T, data_laptop$Gem.HR.T)
t.test(data_vr$AVG_HR_DIFF, data_laptop$AVG_HR_DIFF)
#Histogram aantal fouten
# Laptop baseline
ggplot(data_laptop, aes(x = Fouten.B)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Aantal keren dat een bepaald aantal fouten is gemaakt baseline test Laptop",
    x = "Aantal fouten",
    y = "Aantal keren dat de fout is gemaakt"
  ) +
  theme_minimal()+ 
  coord_cartesian(ylim = c(0, 20),
                  xlim = c(0, 4.25))

#VR baseline
ggplot(data_vr, aes(x = Fouten.B)) +
  geom_histogram(binwidth = 1, fill = "red", color = "white") +
  labs(
    title = "Aantal keren dat een bepaald aantal fouten is gemaakt baseline test VR",
    x = "Aantal fouten",
    y = "Aantal keren dat de fout is gemaakt"
  ) +
  theme_minimal()+ 
  coord_cartesian(ylim = c(0, 20),
                  xlim = c(0, 4.5))


# Laptop Tijdsdruk
ggplot(data_laptop, aes(x = Fouten.T)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Aantal keren dat een bepaald aantal fouten is gemaakt tijdsdruk test Laptop",
    x = "Aantal fouten",
    y = "Aantal keren dat de fout is gemaakt"
  ) +
  theme_minimal()+ 
  coord_cartesian(ylim = c(0, 20),
                  xlim = c(0, 4.25))

# VR tijdsdruk
ggplot(data_vr, aes(x = Fouten.T)) +
  geom_histogram(binwidth = 1, fill = "red", color = "white") +
  labs(
    title = "Aantal keren dat een bepaald aantal fouten is gemaakt tijdsdruk test VR",
    x = "Aantal fouten",
    y = "Aantal keren dat de fout is gemaakt"
  ) +
  theme_minimal()+ 
  coord_cartesian(ylim = c(0, 20),
                  xlim = c(0, 4.25))


# Power analysis
library(pwr)
pwr.t.test(d = 0.044, n = 33, sig.level = 0.05, type = "two.sample")

# Gemiddelde hartslag tests
data_vr$Gem.HR.Diff <- data_vr$Gem.HR.T - data_vr$Gem.HR.B
data_laptop$Gem.HR.Diff <- data_laptop$Gem.HR.T - data_laptop$Gem.HR.B

# VR
describe(data_vr$Gem.HR.T)
describe(data_vr$Gem.HR.B)
describe(data_vr$Gem.HR.Diff)

# Laptop
describe(data_laptop$Gem.HR.T)
describe(data_laptop$Gem.HR.B)
describe(data_laptop$Gem.HR.Diff)

# Power
pwr.t.test(d = 0.328, n = 33, sig.level = 0.05, type = "two.sample")

# T test
t.test(data_vr$Gem.HR.Diff, data_laptop$Gem.HR.Diff)

# Descriptives
describe(data_vr)
describe(data_laptop)


# AVG HR graph
data_vr$AVG_HR_DIFF <- data_vr$Gem.HR.T - data_vr$Gem.HR.B
data_laptop$AVG_HR_DIFF <- data_laptop$Gem.HR.T - data_laptop$Gem.HR.B

#VR
ggplot() +
  geom_point(data = data_vr, aes(x = factor(Participant), y = AVG_HR_DIFF), color = 'red') +
  geom_point(data = data_laptop, aes(x = factor(Participant), y = AVG_HR_DIFF), color = 'steelblue')
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_segment(aes(x = factor(Participant), y = AVG_HR_DIFF, xend = factor(Participant), yend = 0), color = "red", linetype = "dotted") +
  labs(title = "Puntgrafiek met scheidingslijn", x = "X-as", y = "Y-as")

  
ggplot() +
  geom_point(data = data_vr, aes(x = factor(Participant), y = AVG_HR_DIFF), color = "blue") +
  geom_point(data = data_laptop, aes(x = factor(Participant), y = AVG_HR_DIFF), color = "red") +
  geom_hline(yintercept = 0, linetype = "solid", color = "green", size = 1.5) +
  labs(title = "Puntgrafiek met twee datasets", x = "X-as", y = "Y-as")

