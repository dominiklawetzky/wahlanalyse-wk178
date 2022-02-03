##### Preamble -----

rm(list = ls())

# Working Directory
setwd("/Users/dominiklawetzky/Documents/GitHub/wahlanalyse-wk178")

## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "multcomp", "tidyr", "knitr", "car", "psych", "tidyverse", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "shiny", "gvlma", "emmeans", "ez")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(ggthemes)


## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))


##### Daten laden -----

wk178 <- read.csv("wk178.csv", header = TRUE, sep = ";")

# ABSOLUTE Erststimmen

wk178_erststimme <- data.frame(gebiet = wk178$gebiet.name,
                               wahlberechtigte = wk178$wahlberechtigte,
                               waehler = wk178$waehler,
                               ungueltig = wk178$ungueltig.erststimme,
                               gueltig = wk178$gueltig.erststimme,
                               CDU = wk178$erststimme.cdu,
                               SPD = wk178$erststimme.spd,
                               AfD = wk178$erststimme.afd,
                               FDP = wk178$erststimme.fdp,
                               Grüne = wk178$erststimme.gruene,
                               Linke = wk178$erststimme.linke,
                               "Freie Wähler" = wk178$erststimme.fw,
                               "Die Basis" = wk178$erststimme.basis,
                               check.names = FALSE
                              )


# ABSOLUTE Zweitstimmen

wk178_zweitstimme <- data.frame(gebiet = wk178$gebiet.name,
                                wahlberechtigte = wk178$wahlberechtigte,
                                waehler = wk178$waehler,
                                ungueltig = wk178$ungueltig.zweitstimme,
                                gueltig = wk178$gueltig.zweitstimme,
                                CDU = wk178$zweitstimme.cdu,
                                SPD = wk178$zweitstimme.spd,
                                AfD = wk178$zweitstimme.afd,
                                FDP = wk178$zweitstimme.fdp,
                                Grüne = wk178$zweitstimme.gruene,
                                Linke = wk178$zweitstimme.linke,
                                "Freie Wähler" = wk178$zweitstimme.fw,
                                "Die Basis" = wk178$zweitstimme.basis,
                                NPD = wk178$zweitstimme.npd,
                                check.names = FALSE
                                )

# RELATIVE Erststimmen

wk178_erststimme_prozent <- wk178_erststimme[6:13]/wk178_erststimme$gueltig 
wk178_erststimme_prozent$Andere <- 1-rowSums(wk178_erststimme_prozent)
wk178_erststimme_prozent$Gebiet <- wk178_erststimme$gebiet
wk178_erststimme_prozent$Stimmen <- wk178_erststimme$gueltig


# RELATIVE Zweitstimmen

wk178_zweitstimme_prozent <- wk178_zweitstimme[6:14]/wk178_zweitstimme$gueltig
wk178_zweitstimme_prozent$Andere <- 1-rowSums(wk178_zweitstimme_prozent)
wk178_zweitstimme_prozent$Gebiet <- wk178_zweitstimme$gebiet
wk178_zweitstimme_prozent$Stimmen <- wk178_zweitstimme$gueltig

# Wahlberechtigte
wahlberechtigte <- data.frame(anzahl = wk178_erststimme$wahlberechtigte,
                              gebiet = wk178_erststimme$gebiet,
                              gueltig = wk178_erststimme$gueltig)


# GESAMT Erststimme

wk178_erststimme_gesamt <- data.frame(wahlberechtigte = sum(wk178$wahlberechtigte),
                                      waehler = sum(wk178$waehler),
                                      ungueltig = sum(wk178$ungueltig.erststimme),
                                      gueltig = sum(wk178$gueltig.erststimme),
                                      CDU = sum(wk178$erststimme.cdu)/sum(wk178$gueltig.erststimme),
                                      SPD = sum(wk178$erststimme.spd)/sum(wk178$gueltig.erststimme),
                                      AfD = sum(wk178$erststimme.afd)/sum(wk178$gueltig.erststimme),
                                      FDP = sum(wk178$erststimme.fdp)/sum(wk178$gueltig.erststimme),
                                      Grüne = sum(wk178$erststimme.gruene)/sum(wk178$gueltig.erststimme),
                                      Linke = sum(wk178$erststimme.linke)/sum(wk178$gueltig.erststimme),
                                      "Freie Wähler" = sum(wk178$erststimme.fw)/sum(wk178$gueltig.erststimme),
                                      "Die Basis" = sum(wk178$erststimme.basis)/sum(wk178$gueltig.erststimme),
                                      check.names = FALSE)

wk178_erststimme_gesamt$Andere <- 1-sum(wk178_erststimme_gesamt[5:12])

# GESAMT Zweitstimme

wk178_zweitstimme_gesamt <- data.frame(wahlberechtigte = sum(wk178$wahlberechtigte),
                                       waehler = sum(wk178$waehler),
                                       ungueltig = sum(wk178$ungueltig.zweitstimme),
                                       gueltig = sum(wk178$gueltig.zweitstimme),
                                       CDU = sum(wk178$zweitstimme.cdu)/sum(wk178$gueltig.zweitstimme),
                                       SPD = sum(wk178$zweitstimme.spd)/sum(wk178$gueltig.zweitstimme),
                                       AfD = sum(wk178$zweitstimme.afd)/sum(wk178$gueltig.zweitstimme),
                                       FDP = sum(wk178$zweitstimme.fdp)/sum(wk178$gueltig.zweitstimme),
                                       Grüne = sum(wk178$zweitstimme.gruene)/sum(wk178$gueltig.zweitstimme),
                                       Linke = sum(wk178$zweitstimme.linke)/sum(wk178$gueltig.zweitstimme),
                                       "Freie Wähler" = sum(wk178$zweitstimme.fw)/sum(wk178$gueltig.zweitstimme),
                                       "Die Basis" = sum(wk178$zweitstimme.basis)/sum(wk178$gueltig.zweitstimme),
                                       NPD = sum(wk178$zweitstimme.npd)/sum(wk178$gueltig.zweitstimme),
                                       check.names = FALSE)

wk178_zweitstimme_gesamt$Andere <- 1-sum(wk178_zweitstimme_gesamt[5:12])


# Umwandlung Longformat

wk178_erststimme_prozent_long <- wk178_erststimme_prozent %>%
  pivot_longer(!c(Gebiet, Stimmen), names_to = "Partei", values_to = "Prozent")

wk178_zweitstimme_prozent_long <- wk178_zweitstimme_prozent %>%
  pivot_longer(!c(Gebiet, Stimmen), names_to = "Partei", values_to = "Prozent")

wk178_erststimme_gesamt_long <- wk178_erststimme_gesamt[, 4:13] %>%
  pivot_longer(!gueltig, names_to = "Partei", values_to = "Prozent")

wk178_zweitstimme_gesamt_long <- wk178_zweitstimme_gesamt[, 4:14] %>%
  pivot_longer(!gueltig, names_to = "Partei", values_to = "Prozent")

##### Voreinstellungen -----

colors <- c("CDU" = "#000000",
            "SPD" = "#e2001a",
            "Grüne" = "#1ca42c",
            "FDP" = "#fbeb04", 
            "AfD" = "#019ee3",
            "Linke" = "#bd3076", 
            "Freie Wähler" = "#2596be",
            "Die Basis" = "#f49404",
            "NPD" = "#e40424",
            "Andere" = "#7c7c7c")

level_order <- c("CDU", "SPD", "Grüne", "FDP", "AfD", "Linke", "Freie Wähler", "Die Basis", "NPD", "Andere")


##### Plot 1: Zweitstimmen -----

plot1 <- ggplot(data = wk178_zweitstimme_prozent_long) +
  geom_point(aes(x = factor(Partei, levels = level_order), y = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
  # geom_Point(data = )
  labs(title = "Streuung der Zweitstimmen-Ergebnisse nach Gebieten", 
       subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
       y = "Prozent",
       alpha = "Gültige Stimmen",
       size = "Gültige Stimmen",
       caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
  scale_color_manual(name = "Parteien", values = colors, guide = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=rel(1.2), 
                                 angle=90, 
                                 margin = margin(b = 12)),
        axis.text.y = element_text(size=rel(1.2)))
        legend.text = element_text(size=rel(1.3)),
        legend.title = element_text(size=rel(1.3)),
        plot.subtitle = element_text(size=rel(1.3))) +
  theme(plot.title = element_text(size = 22, 
                                  face = "bold")) +
  theme(legend.position="bottom")
  
plot1

##### Plot 2: Erststimmen -----

plot2 <- ggplot(data = wk178_erststimme_prozent_long) +
  geom_point(aes(x = factor(Partei, levels = level_order), y = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
  # geom_Point(data = )
  labs(title = "Streuung der Erststimmen-Ergebnisse nach Gebieten", 
       subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
       color = "Parteien",
       y = "Prozent",
       alpha = "Gültige Stimmen",
       size = "Gültige Stimmen",
       caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
  scale_color_manual(name = "Parteien", values = colors, guide = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=rel(1.2), 
                                   angle=90, 
                                   margin = margin(b = 12)),
        axis.text.y = element_text(size=rel(1.2)))
        legend.text = element_text(size=rel(1.3)),
        legend.title = element_text(size=rel(1.3)),
  plot.subtitle = element_text(size=rel(1.3))) +
        theme(plot.title = element_text(size = 22, 
                                        face = "bold")) +
  theme(legend.position="bottom")

plot2

##### Plot 3: GRÜNE Zweitstimmen nach Gebiet -----

plot3 <- ggplot(data = filter(wk178_zweitstimme_prozent_long, Partei %in% "Grüne")) +
  geom_point(aes(y = Gebiet, x = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
  geom_vline(xintercept = wk178_zweitstimme_gesamt$Grüne, 
             color = "#1ca42c",
             size = 1) +
  geom_text(data = wk178_zweitstimme_gesamt, 
            aes(x = Grüne,
                y = 3.325,
                label = "Gesamtergebnis",
                hjust = -.125,
                vjust = 0,
                angle = 0,
                fontface = 2), 
            color = "#1ca42c",
            size = 4.5) +
  labs(title = "GRÜNES Zweitstimmen-Ergebnis \nnach Gebieten", 
       subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
       y = "Prozent",
       alpha = "Gültige Stimmen",
       size = "Gültige Stimmen",
       caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
  scale_color_manual(name = "Parteien", values = colors, guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=rel(1.2), 
                                   angle=0, 
                                   margin = margin(b = 12)),
        axis.text.y = element_text(size=rel(1.2)))
        legend.text = element_text(size=rel(1.3)),
        legend.title = element_text(size=rel(1.3)),
        plot.subtitle = element_text(size=rel(1.3))) +
          theme(plot.title = element_text(size = 22, 
                                          face = "bold",
                                          hjust = .5)) +
          theme(legend.position="bottom")

plot3

##### Plot 4: Erststimme nach Gebiet -----

plot4 <- ggplot(data = wk178_erststimme_prozent_long) +
                geom_point(aes(y = Gebiet, x = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
                geom_vline(data = wk178_erststimme_gesamt_long,
                           aes(xintercept = Prozent, color = Partei),
                           linetype = "dashed",
                           size = .8) +
                labs(title = "Erststimmen-Ergebnis \nnach Gebieten", 
                     subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
                     y = "Prozent",
                     alpha = "Gültige Stimmen",
                     size = "Gültige Stimmen",
                     caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
                scale_color_manual(name = "Parteien", values = colors, guide = "none") +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                theme_fivethirtyeight() +
                theme(axis.text.x = element_text(size=rel(1.2), 
                                                 angle=0, 
                                                 margin = margin(b = 12)),
                      axis.text.y = element_text(size=rel(1.2)))
              legend.text = element_text(size=rel(1.3)),
              legend.title = element_text(size=rel(1.3)),
              plot.subtitle = element_text(size=rel(1.3))) +
                theme(plot.title = element_text(size = 22, 
                                                face = "bold",
                                                hjust = .5)) +
                theme(legend.position="bottom")

plot4


##### Plot 5: Zweitstimme nach Gebiet -----

plot5 <- ggplot(data = wk178_zweitstimme_prozent_long) +
                geom_point(aes(y = Gebiet, x = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
                geom_vline(data = wk178_zweitstimme_gesamt_long,
                           aes(xintercept = Prozent, color = Partei),
                           linetype = "dashed",
                           size = .8) +
                labs(title = "Zweitstimmen-Ergebnis \nnach Gebieten", 
                     subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
                     y = "Prozent",
                     alpha = "Gültige Stimmen",
                     size = "Gültige Stimmen",
                     caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
                scale_color_manual(name = "Parteien", values = colors, guide = "none") +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                theme_fivethirtyeight() +
                theme(axis.text.x = element_text(size=rel(1.2), 
                                                 angle=0, 
                                                 margin = margin(b = 12)),
                      axis.text.y = element_text(size=rel(1.2)))
              legend.text = element_text(size=rel(1.3)),
              legend.title = element_text(size=rel(1.3)),
              plot.subtitle = element_text(size=rel(1.3))) +
                theme(plot.title = element_text(size = 22, 
                                                face = "bold",
                                                hjust = .5)) +
                theme(legend.position="bottom")

plot5

##### Plot 6: Stimmensplitting -----


wk178_zweitstimme_alt <- subset(wk178_zweitstimme, select = -c(NPD))

wk178_zweitstimme_prozent_alt <- wk178_zweitstimme_alt[6:13]/wk178_zweitstimme_alt$gueltig
wk178_zweitstimme_prozent_alt$Andere <- 1-rowSums(wk178_zweitstimme_prozent_alt)
wk178_zweitstimme_prozent_alt$Gebiet <- wk178_zweitstimme_alt$gebiet
wk178_zweitstimme_prozent_alt$Stimmen <- wk178_zweitstimme_alt$gueltig

wk178_zweitstimme_prozent_long_alt <- wk178_zweitstimme_prozent_alt %>%
  pivot_longer(!c(Gebiet, Stimmen), names_to = "Partei", values_to = "Prozent")

wk178_stimmsplitting_prozent_long <- data.frame(Gebiet = wk178_zweitstimme_prozent_long_alt$Gebiet,
                                                Splitting = wk178_erststimme_prozent_long$Prozent-wk178_zweitstimme_prozent_long_alt$Prozent,
                                                Partei = wk178_zweitstimme_prozent_long_alt$Partei,
                                                Stimmen = wk178_zweitstimme_prozent_long_alt$Stimmen)

plot6 <- ggplot(data = wk178_stimmsplitting_prozent_long) +
                geom_bar(aes(y = Splitting, x = factor(Partei, levels = level_order), color = Partei, fill = Partei, alpha = Stimmen), stat = "identity") +
                labs(title = "Stimmen-Splitting nach Gebieten", 
                     subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
                     y = "Prozent",
                     alpha = "Gültige Stimmen",
                     size = "Gültige Stimmen",
                     caption = "Dargestellt ist die Differenz zwischen Erst- und Zweitstimmen;\npositive Prozentwerte bedeuten mehr Erst- als Zweitstimmen\n\ngithub.com/dominiklawetzky/wahlanalyse-wk178") +
                facet_wrap(~ Gebiet, ncol = 2) + 
                scale_color_manual(name = "Parteien", values = colors, guide = "none") +
                scale_fill_manual(name = "Parteien", values = colors, guide = "none") +
                scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                theme_fivethirtyeight() +
                theme(axis.text.x = element_text(size=rel(1), 
                                                 angle=90, 
                                                 margin = margin(b = 12)),
                      axis.text.y = element_text(size=rel(1.2)))
              legend.text = element_text(size=rel(1.3)),
              legend.title = element_text(size=rel(1.3)),
              plot.subtitle = element_text(size=rel(1.3))) +
                theme(plot.title = element_text(size = 22, 
                                                face = "bold",
                                                hjust = .5)) +
                theme(legend.position="bottom")

plot6

ggsave(file="plot6.png", plot=plot6, width=8, height=30)


##### Plot 7: Zweitstimme LINKE -----

install.packages("gghighlight")
library(gghighlight)

plot7 <- ggplot(data = filter(wk178_zweitstimme_prozent_long, Partei %in% "Linke")) +
                geom_point(aes(y = Gebiet, x = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
                geom_vline(xintercept = wk178_zweitstimme_gesamt$Linke, 
                           color = "#bd3076",
                           size = 1) +
                geom_text(data = wk178_zweitstimme_gesamt, 
                          aes(x = Linke,
                              y = 3.325,
                              label = "Gesamtergebnis",
                              hjust = 1.2,
                              vjust = 0,
                              angle = 0,
                              fontface = 2), 
                          color = "#bd3076",
                          size = 4.5) +
                gghighlight(Prozent > wk178_zweitstimme_gesamt$Linke) +
                labs(title = "Zweitstimmen-Ergebnis der LINKEN\nnach Gebieten", 
                     subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
                     y = "Prozent",
                     alpha = "Gültige Stimmen",
                     size = "Gültige Stimmen",
                     caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
                scale_color_manual(name = "Parteien", values = colors, guide = "none") +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                theme_fivethirtyeight() +
                theme(axis.text.x = element_text(size=rel(1.2), 
                                                 angle=0, 
                                                 margin = margin(b = 12)),
                                    axis.text.y = element_text(size=rel(1.2)), 
                                    legend.text = element_text(size=rel(1.3)),
                                    legend.title = element_text(size=rel(1.3)),
                                    plot.subtitle = element_text(size=rel(1.3))) +
                theme(plot.title = element_text(size = 22, 
                                                face = "bold",
                                                hjust = .5)) +
                theme(legend.position="bottom")

plot7


##### Plot 8: Zweitstimme CDU -----


plot8 <- ggplot(data = filter(wk178_zweitstimme_prozent_long, Partei %in% "CDU")) +
  geom_point(aes(y = Gebiet, x = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
  geom_vline(xintercept = wk178_zweitstimme_gesamt$CDU, 
             color = "#000000",
             size = 1) +
  geom_text(data = wk178_zweitstimme_gesamt, 
            aes(x = CDU,
                y = 3.325,
                label = "Gesamtergebnis",
                hjust = -.1,
                vjust = 0,
                angle = 0,
                fontface = 2), 
            color = "#000000",
            size = 4.5) +
  labs(title = "Zweitstimmen-Ergebnis der CDU\nnach Gebieten", 
       subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
       y = "Prozent",
       alpha = "Gültige Stimmen",
       size = "Gültige Stimmen",
       caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
  scale_color_manual(name = "Parteien", values = colors, guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=rel(1.2), 
                                   angle=0, 
                                   margin = margin(b = 12)),
        axis.text.y = element_text(size=rel(1.2)), 
        legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        plot.subtitle = element_text(size=rel(1.2))) +
  theme(plot.title = element_text(size = 18, 
                                  face = "bold",
                                  hjust = 0)) +
  theme(legend.position="bottom")

plot8


##### Plot 10: Zweitstimme FDP -----

which.max(filter(wk178_zweitstimme_prozent_long, Partei %in% "FDP")$Prozent)

filter(wk178_zweitstimme_prozent_long, Partei %in% "FDP")[11,]

filter(wk178_zweitstimme_prozent_long, Gebiet %in% "Gemeinde Schlangenbad")

plot10 <- ggplot(data = filter(wk178_zweitstimme_prozent_long, Partei %in% "FDP")) +
  geom_point(aes(y = Gebiet, x = Prozent, color = Partei, alpha = Stimmen, size = Stimmen), stat = "identity") +
  geom_vline(xintercept = wk178_zweitstimme_gesamt$FDP, 
             color = "#fbce04",
             size = 1) +
  geom_text(data = wk178_zweitstimme_gesamt, 
            aes(x = FDP,
                y = 3.325,
                label = "Gesamtergebnis",
                hjust = -.1,
                vjust = 0,
                angle = 0,
                fontface = 2), 
            color = "#fbce04",
            size = 4.5) +
  labs(title = "Zweitstimmen-Ergebnis der FDP\nnach Gebieten", 
       subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
       y = "Prozent",
       alpha = "Gültige Stimmen",
       size = "Gültige Stimmen",
       caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
  scale_color_manual(name = "Parteien", values = c("#fbce04"), guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=rel(1.2), 
                                   angle=0, 
                                   margin = margin(b = 12)),
        axis.text.y = element_text(size=rel(1.2)), 
        legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        plot.subtitle = element_text(size=rel(1.2))) +
  theme(plot.title = element_text(size = 18, 
                                  face = "bold",
                                  hjust = 0)) +
  theme(legend.position="bottom")

plot10

##### Plot 11: Gesamtergebnis Erststimmen -----

plot11 <- ggplot(data = wk178_erststimme_gesamt_long) +
  geom_bar(aes(x = factor(Partei, levels = level_order), y = Prozent, color = Partei, fill = Partei), stat = "identity") +
  labs(title = "Prozentuales Ergebnis der Erststimmen", 
       subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
       y = "Prozent",
       caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
  scale_color_manual(name = "Parteien", values = colors, guide = "none") +
  scale_fill_manual(name = "Parteien", values = colors, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=rel(1.2), 
                                   angle=0, 
                                   margin = margin(b = 12)),
        axis.text.y = element_text(size=rel(1.2)), 
        legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        plot.subtitle = element_text(size=rel(1.2))) +
  theme(plot.title = element_text(size = 18, 
                                  face = "bold",
                                  hjust = 0)) +
  theme(legend.position="bottom")

plot11


##### Plot 12: Gesamtergebnis Zweitstimmen -----

plot12 <- ggplot(data = wk178_zweitstimme_gesamt_long) +
  geom_bar(aes(x = factor(Partei, levels = level_order), y = Prozent, color = Partei, fill = Partei), stat = "identity") +
  labs(title = "Prozentuales Ergebnis der Zweitstimmen", 
       subtitle = "Wahlkreis 178 (Rheingau-Taunus — Limburg)",
       y = "Prozent",
       caption = "github.com/dominiklawetzky/wahlanalyse-wk178") +
  scale_color_manual(name = "Parteien", values = colors, guide = "none") +
  scale_fill_manual(name = "Parteien", values = colors, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=rel(1.2), 
                                   angle=0, 
                                   margin = margin(b = 12)),
        axis.text.y = element_text(size=rel(1.2)), 
        legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        plot.subtitle = element_text(size=rel(1.2))) +
  theme(plot.title = element_text(size = 18, 
                                  face = "bold",
                                  hjust = 0)) +
  theme(legend.position="bottom")

plot12

wk178_erststimme_gesamt_long
