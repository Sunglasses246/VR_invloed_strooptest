# Load necessary library
library(dplyr)
library(ggplot2)
library(psych)
library(lsr)
library(effsize)
library(rstatix)
library(MBESS)
library(boot)
library(writexl)
library(coin)

data_melih <- Dataset.OIS...Dataset
data_melih$TijdDifference <- data_melih$Tijdsduur.T - data_melih$Tijdsduur.B
data_melih$FoutenDifference <- data_melih$Fouten.T - data_melih$Fouten.B
data_melih$GemHRDifference <- data_melih$Gem.HR.T - data_melih$Gem.HR.B

# Onafhankelijke t-test voor verschil in tijdsduur tussen baseline en tijdsdruk tussen VR en Laptop groepen
t.test(TijdDifference ~ Type.test, data = data_melih)

# Onafhankelijke t-test voor verschil in fouten tussen baseline en tijdsdruk tussen VR en Laptop groepen
t.test(FoutenDifference ~ Type.test, data = data_melih)

# Onafhankelijke t-test voor verschil in hartslag tussen baseline en tijdsdruk tussen VR en Laptop groepen
t.test(GemHRDifference ~ Type.test, data = data_melih)

# Gepaarde t-test voor Tijdsduur in VR Groep
t.test(data_melih$Tijdsduur.B[data_melih$Type.test == "VR"], 
       data_melih$Tijdsduur.T[data_melih$Type.test == "VR"], 
       paired = TRUE)

# Gepaarde t-test voor Tijdsduur in Laptop Groep
t.test(data_melih$Tijdsduur.B[data_melih$Type.test == "Laptop"], 
       data_melih$Tijdsduur.T[data_melih$Type.test == "Laptop"], 
       paired = TRUE)

# Gepaarde t-test voor Fouten in VR Groep
t.test(data_melih$Fouten.B[data_melih$Type.test == "VR"], 
       data_melih$Fouten.T[data_melih$Type.test == "VR"], 
       paired = TRUE)

# Gepaarde t-test voor Fouten in Laptop Groep
t.test(data_melih$Fouten.B[data_melih$Type.test == "Laptop"], 
       data_melih$Fouten.T[data_melih$Type.test == "Laptop"], 
       paired = TRUE)

# Correlatie in VR Groep
cor.test(data_melih$Tijdsduur.B[data_melih$Type.test == "VR"], 
         data_melih$Fouten.B[data_melih$Type.test == "VR"])

# Correlatie in Laptop Groep
cor.test(data_melih$Tijdsduur.B[data_melih$Type.test == "Laptop"], 
         data_melih$Fouten.B[data_melih$Type.test == "Laptop"])

# Gemiddelde hartslag in VR Groep
t.test(data_melih$Gem.HR.B[data_melih$Type.test == "VR"], 
       data_melih$Gem.HR.T[data_melih$Type.test == "VR"], 
       paired = TRUE)

# Gemiddelde hartslag in Laptop Groep
t.test(data_melih$Gem.HR.B[data_melih$Type.test == "Laptop"], 
       data_melih$Gem.HR.T[data_melih$Type.test == "Laptop"], 
       paired = TRUE)

# Lineaire regressieanalyse met Tijdsduur als afhankelijke variabele
lm.model.tijdsduur <- lm(Tijdsduur.B ~ Type.test + Leeftijd + Geslacht, data = data_melih)
summary(lm.model.tijdsduur)

# Lineaire regressieanalyse met Fouten als afhankelijke variabele
lm.model.fouten <- lm(Fouten.B ~ Type.test + Leeftijd + Geslacht, data = data_melih)
summary(lm.model.fouten)

# Overzicht van alle variabelen
data_melih %>%
  select(Leeftijd, Tijdsduur.B, Tijdsduur.T, Fouten.B, Fouten.T, Gem.HR.B, Gem.HR.T) %>%
  describe()

# Shapiro-Wilk-test voor Tijdsduur.T in VR-groep
shapiro.test(data_melih$Tijdsduur.T[data_melih$Type.test == "VR"])

# Shapiro-Wilk-test voor Tijdsduur.T in Laptop-groep
shapiro.test(data_melih$Tijdsduur.T[data_melih$Type.test == "Laptop"])

# Shapiro-Wilk-test voor Fouten.T in VR-groep
shapiro.test(data_melih$Fouten.T[data_melih$Type.test == "VR"])

# Shapiro-Wilk-test voor Fouten.T in Laptop-groep
shapiro.test(data_melih$Fouten.T[data_melih$Type.test == "Laptop"])

# Tijdsduur.B in VR-groep
shapiro.test(data_melih$Tijdsduur.B[data_melih$Type.test == "VR"])

# Tijdsduur.B in Laptop-groep
shapiro.test(data_melih$Tijdsduur.B[data_melih$Type.test == "Laptop"])

# Fouten.B in VR-groep
shapiro.test(data_melih$Fouten.B[data_melih$Type.test == "VR"])

# Fouten.B in Laptop-groep
shapiro.test(data_melih$Fouten.B[data_melih$Type.test == "Laptop"])

# Gem.HR.B in VR-groep
shapiro.test(data_melih$Gem.HR.B[data_melih$Type.test == "VR"])

# Gem.HR.B in Laptop-groep
shapiro.test(data_melih$Gem.HR.B[data_melih$Type.test == "Laptop"])

# Gem.HR.T in VR-groep
shapiro.test(data_melih$Gem.HR.T[data_melih$Type.test == "VR"])

# Gem.HR.T in Laptop-groep
shapiro.test(data_melih$Gem.HR.T[data_melih$Type.test == "Laptop"])

# Cohen's d voor Tijdsduur.T tussen VR en Laptop groepen
d_tijdsduurdif <- cohensD(TijdDifference ~ Type.test, data_melih = data_melih)

# Cohen's d voor Fouten.T tussen VR en Laptop groepen
d_foutendif <- cohensD(FoutenDifference ~ Type.test, data_melih = data_melih)

# Cohen's d voor Gem.HR.T tussen VR en Laptop groepen
d_hrdif <- cohensD(GemHRDifference ~ Type.test, data_melih = data_melih)

# Cohen's d voor gepaarde t-tests vereist een iets andere aanpak
# Voor gepaarde t-tests, bereken eerst de verschilscores
data_melih$Diff_Tijdsduur_VR <- data_melih$Tijdsduur.T[data_melih$Type.test == "VR"] - data_melih$Tijdsduur.B[data_melih$Type.test == "VR"]
data_melih$Diff_Tijdsduur_Laptop <- data_melih$Tijdsduur.T[data_melih$Type.test == "Laptop"] - data_melih$Tijdsduur.B[data_melih$Type.test == "Laptop"]
data_melih$Diff_Fouten_VR <- data_melih$Fouten.T[data_melih$Type.test == "VR"] - data_melih$Fouten.B[data_melih$Type.test == "VR"]
data_melih$Diff_Fouten_Laptop <- data_melih$Fouten.T[data_melih$Type.test == "Laptop"] - data_melih$Fouten.B[data_melih$Type.test == "Laptop"]
data_melih$Diff_HR_VR <- data_melih$Gem.HR.T[data_melih$Type.test == "VR"] - data_melih$Gem.HR.B[data_melih$Type.test == "VR"]
data_melih$Diff_HR_Laptop <- data_melih$Gem.HR.T[data_melih$Type.test == "Laptop"] - data_melih$Gem.HR.B[data_melih$Type.test == "Laptop"]

# Nu berekenen we Cohen's d voor deze verschilscores
d_tijdsduur_VR <- cohensD(data_melih$Diff_Tijdsduur_VR)
d_tijdsduur_Laptop <- cohensD(data_melih$Diff_Tijdsduur_Laptop)
d_fouten_VR <- cohensD(data_melih$Diff_Fouten_VR)
d_fouten_Laptop <- cohensD(data_melih$Diff_Fouten_Laptop)
d_hr_VR <- cohensD(data_melih$Diff_HR_VR)
d_hr_Laptop <- cohensD(data_melih$Diff_HR_Laptop)

# Uitvoer van de berekende Cohen's d waarden
d_tijdsduurdif
d_foutendif
d_hrdif
d_tijdsduur_VR
d_tijdsduur_Laptop
d_fouten_VR
d_fouten_Laptop
d_hr_VR
d_hr_Laptop


#------------------------------------------------------------------------------------------------------------------------------------------------
#Enquete
data_melih_laptop_questions <- Dataset.OIS...Vragenlijst.Laptop
data_melih_vr_questions <- `Dataset.OIS...Vragenlijst.VR.(1)`
# Calculate Cronbach's Alpha
cols_laptop <- c("Zorgt.deze.situatie.voor.spanning.bij.mij.", "Maakt.deze.situatie.u.angstig.", "Hoe.bedreigend.is.deze.situatie.", "In.hoeverre.ervaart.u.deze.situatie.als.stressvol.","Beschikt.u.over.de.vaardigheden.die.nodig.zijn.om.een.succesvol.resultaat.in.deze.situatie.te.bereiken.", "In.welke.mate.heeft.u.tijdsdruk.ervaren.", "In.hoeverre.heeft.tijdsdruk.uw.score.beïnvloed.")
cols_vr <- c("Zorgt.deze.situatie.voor.spanning.bij.mij.", "Maakt.deze.situatie.u.angstig.", "Hoe.bedreigend.is.deze.situatie.", "In.hoeverre.ervaart.u.deze.situatie.als.stressvol.","Beschikt.u.over.de.vaardigheden.die.nodig.zijn.om.een.succesvol.resultaat.in.deze.situatie.te.bereiken.", "In.welke.mate.heeft.u.tijdsdruk.ervaren.", "In.hoeverre.heeft.tijdsdruk.uw.score.beïnvloed.")
alpha_laptop <- alpha(data_melih_laptop_questions[, cols_laptop], check.keys=TRUE)
alpha_vr <- alpha(data_melih_vr_questions[, cols_vr], check.keys=TRUE)
# Print the results
print(alpha_laptop$total)
print(alpha_vr$total)

# Perform Shapiro-Wilk normality test on all numeric columns
shapiro_results_laptop <- lapply(data_melih_laptop_questions, function(x) if(is.numeric(x)) shapiro.test(x))
shapiro_results_vr <- lapply(data_melih_vr_questions, function(x) if(is.numeric(x)) shapiro.test(x))

# Remove NULLs from the list (for non-numeric columns)
shapiro_results_laptop <- shapiro_results_laptop[!sapply(shapiro_results_laptop, is.null)]
shapiro_results_vr <- shapiro_results_vr[!sapply(shapiro_results_vr, is.null)]

# Print the results
print(shapiro_results_laptop)
print(shapiro_results_vr)

# Uitvoeren van de Mann-Whitney U-test met het coin package
results <- list()
for (col in cols_laptop) {
  # Combineer de data_melih van beide groepen in één data_melihframe
  combined_data_melih <- data_melih.frame(
    value = c(data_melih_laptop_questions[[col]], data_melih_vr_questions[[col]]),
    group = factor(c(rep("Laptop", nrow(data_melih_laptop_questions)), rep("VR", nrow(data_melih_vr_questions))))
  )
  
  # Zorg ervoor dat NA-waarden worden verwijderd
  combined_data_melih <- na.omit(combined_data_melih)
  
  # Voer de test uit
  test_result <- wilcox_test(value ~ group, data_melih = combined_data_melih)
  results[[col]] <- test_result
}

# Resultaten bekijken
results

# Z-scores from the Wilcoxon-Mann-Whitney tests
z_scores <- c(2.2919, -0.94753, -1.428, 1.7656, -0.02101, 1.6997, -0.15387)

# Total number of observations
N <- 66

# Calculating r-values
r_values <- z_scores / sqrt(N)

# Printing r-values
r_values
