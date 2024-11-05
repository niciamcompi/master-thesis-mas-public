
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#
#      	function PPCCalcMultiFactorRegression(StartDate,EndDate,MandatNr,ListIndexNr, Rollperiod, Model="")
#
# 		02.05.2013	OS	Erste Version
# 		05.08.2013  OS  Hinzufügen der Auswahl bestimmter Modelle --> Jensen Regression
# 		22.08.2013  OS  Hinzufügen des Fama-French Modells und des Carhart Modells
# 		05.11.2013	OS  Hinzufügen AGS Modelle
# 		19.04.2016	HHU	Hinzufügen Barra Regressionsmodell
# 		24.03.2017	HHU	FF-Modell global Indizes nicht US Indizes verwenden
#
#      	Bereitstellung der Betas + Alphas der Regression (mit flexiblen Indizes)
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#' @export


PPCCalcMultiFactorRegression <- function(StartDate, EndDate, MandatNr, ListIndexNr, Rollperiod, Model = "") {
  if (PPCMonths(StartDate, EndDate) < Rollperiod) {
    return(NA)
  }

  {
    if (Model == "") {
      MandatLogReturns <- PPCReturnsStetig(MandatNr, StartDate, EndDate)$LogPortfolioReturn
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }
      m_currency <- PPCgetMandateDetails(MandatNr)$Currency
      IndexReturns <- PPCgetIndexReturns(ListIndexNr, m_currency, StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen
      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), IndexReturns$IndexName)
      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate
      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 1 + length(ListIndexNr))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 1 + length(ListIndexNr), 4)
      RF <- NA
      M <- NA
    } else if (Model == "Index2Index") {
      # MandatLogReturns <- PPCReturnsStetig(MandatNr,StartDate,EndDate)$LogPortfolioReturn
      # if(is.na(MandatLogReturns)){return(NA)}
      # m_currency	 <- PPCgetMandateDetails(MandatNr)$Currency
      MandatLogReturns <- PPCgetIndexReturns(MandatNr, curr, StartDate, EndDate)
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }
      MandatLogReturns <- MandatLogReturns$LogRenditen
      IndexReturns <- PPCgetIndexReturns(ListIndexNr, curr, StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen
      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), IndexReturns$IndexName)
      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate
      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 1 + length(ListIndexNr))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 1 + length(ListIndexNr), 4)
      RF <- NA
      M <- NA
    } else if (Model == "Jensen") {
      {if (is.na(ListIndexNr[1])) { # benchmark als marktindex
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE)
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        IndexLogReturns <- ExcessReturns$LogBenchmarkExcess
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", PPCgetMandateDetails(MandatNr)$Benchmark, " (", Bezeichnung[22, Language + 1], ")", sep = "")
      } else { # custom marktindex & risk free
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE, CustomRF = ListIndexNr[1])
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        m_currency <- PPCgetMandateDetails(MandatNr)$Currency
        IndexReturns <- PPCgetIndexReturns(ListIndexNr[2], m_currency, StartDate, EndDate)
        if (all(is.na(IndexReturns))) {
          return(NA)
        }
        IndexLogReturns <- IndexReturns$LogRenditen
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", IndexReturns$IndexName, sep = "")
      }                }

      if (all(is.na(IndexLogReturns))) {
        return(NA)
      }
      Date <- zoo::index(IndexLogReturns)
      cNames <- c(
        paste(as.character(Bezeichnung[93, Language + 1]), " (", as.character(Bezeichnung[94, Language + 1]), ")", sep = ""),
        paste(as.character(Bezeichnung[41, Language + 1]), " (", as.character(Bezeichnung[95, Language + 1]), ")", sep = "")
      )
      StartDatePlot <- ExcessReturns$StartDate
      EndDatePlot <- ExcessReturns$EndDate
      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 2)
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 2, 4)
    } else if (Model == "FF") {
      {if (is.na(ListIndexNr[1])) { # benchmark als marktindex
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE)
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        IndexLogReturns <- ExcessReturns$LogBenchmarkExcess
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", PPCgetMandateDetails(MandatNr)$Benchmark, " (", Bezeichnung[22, Language + 1], ")", sep = "")
      } else { # custom marktindex & risk free
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE, CustomRF = ListIndexNr[1])
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        m_currency <- PPCgetMandateDetails(MandatNr)$Currency
        IndexReturns <- PPCgetIndexReturns(ListIndexNr[2], m_currency, StartDate, EndDate)
        if (all(is.na(IndexReturns))) {
          return(NA)
        }
        IndexLogReturns <- IndexReturns$LogRenditen
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", IndexReturns$IndexName, sep = "")
      }                }
      if (all(is.na(IndexLogReturns))) {
        return(NA)
      }
      # FF_Factors <- PPCgetIndexReturns(c(871,872), "CHF", StartDate, EndDate) -> nicht US sondern global FF-Indizes verwenden.
      FF_Factors <- PPCgetIndexReturns(c(1025, 1026), "CHF", StartDate, EndDate)
      if (all(is.na(FF_Factors))) {
        return(NA)
      }
      IndexLogReturns <- merge(IndexLogReturns, FF_Factors$LogRenditen)

      Date <- zoo::index(IndexLogReturns)
      cNames <- c(
        paste(as.character(Bezeichnung[585, Language + 1]), " (", as.character(Bezeichnung[94, Language + 1]), ")", sep = ""),
        paste(as.character(Bezeichnung[580, Language + 1]), " (", as.character(Bezeichnung[95, Language + 1]), ")", sep = ""),
        paste(FF_Factors$IndexName, " (", as.character(Bezeichnung[587, Language + 1]), ")", sep = "")
      )
      StartDatePlot <- ExcessReturns$StartDate
      EndDatePlot <- ExcessReturns$EndDate
      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 4)
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 4, 4)
    } else if (Model == "Carhart") {
      {if (is.na(ListIndexNr[1])) { # benchmark als marktindex
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE)
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        IndexLogReturns <- ExcessReturns$LogBenchmarkExcess
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", PPCgetMandateDetails(MandatNr)$Benchmark, " (", Bezeichnung[22, Language + 1], ")", sep = "")
      } else { # custom marktindex & risk free
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE, CustomRF = ListIndexNr[1])
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        m_currency <- PPCgetMandateDetails(MandatNr)$Currency
        IndexReturns <- PPCgetIndexReturns(ListIndexNr[2], m_currency, StartDate, EndDate)
        if (all(is.na(IndexReturns))) {
          return(NA)
        }
        IndexLogReturns <- IndexReturns$LogRenditen
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", IndexReturns$IndexName, sep = "")
      }                }
      if (all(is.na(IndexLogReturns))) {
        return(NA)
      }

      Carhart_Factors <- PPCgetIndexReturns(c(871, 872, 750), "CHF", StartDate, EndDate)
      if (all(is.na(Carhart_Factors))) {
        return(NA)
      }
      IndexLogReturns <- merge(IndexLogReturns, Carhart_Factors$LogRenditen)

      Date <- zoo::index(IndexLogReturns)
      cNames <- c(
        paste(as.character(Bezeichnung[589, Language + 1]), sep = ""),
        paste(as.character(Bezeichnung[580, Language + 1]), " (", as.character(Bezeichnung[95, Language + 1]), ")", sep = ""),
        paste(Carhart_Factors$IndexName, sep = "")
      )
      # c(paste(as.character(Bezeichnung[589,Language+1])," (", as.character(Bezeichnung[94,Language+1]),")",sep=""),
      # paste(as.character(Bezeichnung[580,Language+1])," (", as.character(Bezeichnung[95,Language+1]),")",sep=""),
      # paste(Carhart_Factors$IndexName," (", as.character(Bezeichnung[587,Language+1]),")",sep=""))
      StartDatePlot <- ExcessReturns$StartDate
      EndDatePlot <- ExcessReturns$EndDate
      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 5)
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 5, 4)
    } else if (Model == "Barra") {
      {if (is.na(ListIndexNr[1])) { # benchmark als marktindex
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE)
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        IndexLogReturns <- ExcessReturns$LogBenchmarkExcess
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", PPCgetMandateDetails(MandatNr)$Benchmark, " (", Bezeichnung[22, Language + 1], ")", sep = "")
      } else { # custom marktindex & risk free
        ExcessReturns <- PPCExcessReturns(MandatNr, StartDate, EndDate, SHOW_MANDATSBEGINN = FALSE, CustomRF = ListIndexNr[1])
        if (is.na(ExcessReturns[1])) {
          return(NA)
        }
        MandatLogReturns <- ExcessReturns$LogPortfolioExcess
        if (all(is.na(MandatLogReturns))) {
          return(NA)
        }

        m_currency <- PPCgetMandateDetails(MandatNr)$Currency
        IndexReturns <- PPCgetIndexReturns(ListIndexNr[2], m_currency, StartDate, EndDate)
        if (all(is.na(IndexReturns))) {
          return(NA)
        }
        IndexLogReturns <- IndexReturns$LogRenditen
        RF <- paste(Bezeichnung[52, Language + 1], ExcessReturns$IndexName, sep = " ")
        M <- paste(Bezeichnung[580, Language + 1], ": ", IndexReturns$IndexName, sep = "")
      }                }
      if (all(is.na(IndexLogReturns))) {
        return(NA)
      }
      # Value, Leverage, Momentum, Volatility
      Barra_Factors <- PPCgetIndexReturns(c(1056, 1058, 1055, 1057), "CHF", StartDate, EndDate)
      if (all(is.na(Barra_Factors))) {
        return(NA)
      }
      IndexLogReturns <- merge(IndexLogReturns, Barra_Factors$LogRenditen)

      Date <- zoo::index(IndexLogReturns)
      cNames <- c(
        paste("Barra Alpha", " (", as.character(Bezeichnung[94, Language + 1]), ")", sep = ""),
        paste(as.character(Bezeichnung[580, Language + 1]), " (", as.character(Bezeichnung[95, Language + 1]), ")", sep = ""),
        paste(Barra_Factors$IndexName, " (", as.character(Bezeichnung[587, Language + 1]), ")", sep = "")
      )

      StartDatePlot <- ExcessReturns$StartDate
      EndDatePlot <- ExcessReturns$EndDate
      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 6)
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 6, 4)
    } else if (Model == "AGS_Global_Equity") {
      # 736: MSCI World Value; 737: MSCI World Growth; 738: MSCI World Small Cap; 735: MSCI World Large Cap; 750: Momentum; 1702: Volatility
      # ListIndexNr <- c(736,737,738,735,750,1702)
      # 809: AGS Global Value/Growth; 810: AGS Global Small/Large; 1027: Global Momentum; 1702: Volatility
      ListIndexNr <- c(809, 810, 1027, 1702)

      # Bei den anderen Standardfaktormodellen werden Start und EndDate im Grafikfile berechnet
      CommonPeriod <- PPCcalcGemeinsamePeriode(MandatNr, ListIndexNr, NA, StartDate, EndDate)
      StartDate <- CommonPeriod$StartDate
      EndDate <- CommonPeriod$EndDate

      MandatLogReturns <- PPCReturnsStetig(MandatNr, StartDate, EndDate)$LogOutperformance
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }

      IndexReturns <- PPCgetIndexReturns(ListIndexNr, "CHF", StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen

      IndexLogReturns <- merge(
        IndexLogReturns[, 1],
        IndexLogReturns[, 2],
        IndexLogReturns[, 3],
        IndexLogReturns[, 4],
        all = T
      )

      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), c("Value/Growth", "Small/Large", "Momentum", "Volatility"))

      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate

      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(NA, loops, 1 + NCOL(IndexLogReturns))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(NA, 1 + NCOL(IndexLogReturns), 4)
      RF <- NA
      M <- NA
    } else if (Model == "AGS_Equity_CH") {
      # ListIndexNr <- c(741,742,117,50,750,1702)
      # 993: AGS Swiss Value/Growth; 994: AGS Swiss Small/Large; 1027: Global Momentum; 1702: Volatility
      ListIndexNr <- c(811, 812, 1027, 1702)
      # Bei den anderen Standardfaktormodellen werden Start und EndDate im Grafikfile berechnet
      CommonPeriod <- PPCcalcGemeinsamePeriode(MandatNr, ListIndexNr, NA, StartDate, EndDate)
      StartDate <- CommonPeriod$StartDate
      EndDate <- CommonPeriod$EndDate

      MandatLogReturns <- PPCReturnsStetig(MandatNr, StartDate, EndDate)$LogOutperformance
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }

      IndexReturns <- PPCgetIndexReturns(ListIndexNr, "CHF", StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen

      IndexLogReturns <- merge(
        IndexLogReturns[, 1],
        IndexLogReturns[, 2],
        IndexLogReturns[, 3],
        IndexLogReturns[, 4],
        all = T
      )

      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), c("Value/Growth", "Small/Large", "Momentum", "Volatility"))

      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate

      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 1 + NCOL(IndexLogReturns))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 1 + NCOL(IndexLogReturns), 4)
      RF <- NA
      M <- NA
    } else if (Model == "AGS_Global_Bonds") {
      # JPM_GBI, Treasury 20+ , Treasury 1-3m , AA , BBB, HY
      # ListIndexNr <- c(744,743,745,746,747,1702,751)
      # 813: AGS US Term; 814: AGS US Credit AA; 815: AGS US Credit BBB, 816: AGS US Credit HY; 751: AGS Global FX Carry ; 1702: Volatility
      ListIndexNr <- c(813, 814, 815, 816, 1702, 751)

      # Bei den anderen Standardfaktormodellen werden Start und EndDate im Grafikfile berechnet
      CommonPeriod <- PPCcalcGemeinsamePeriode(MandatNr, ListIndexNr, NA, StartDate, EndDate)
      StartDate <- CommonPeriod$StartDate
      EndDate <- CommonPeriod$EndDate

      MandatLogReturns <- PPCReturnsStetig(MandatNr, StartDate, EndDate)$LogOutperformance
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }

      IndexReturns <- PPCgetIndexReturns(ListIndexNr, "CHF", StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen

      # off, on
      # Indizes_Liq <- c(748,749)
      # IndexStand <- PPCgetIndexStand(Indizes_Liq, PPCGetDateOffset(StartDate,-1,0), EndDate)$IndexStand
      # LIQUIDITY <- 100*(as.numeric((IndexStand[1:(NROW(IndexStand)-1),1]-IndexStand[1:(NROW(IndexStand)-1),2])) - as.numeric((IndexStand[2:NROW(IndexStand),1]-IndexStand[2:NROW(IndexStand),2])))
      Indizes_Liq <- c(817)
      IndexStand <- PPCgetIndexStand(Indizes_Liq, PPCGetDateOffset(StartDate, 0, 0), EndDate, silent = TRUE)$IndexStand

      LIQUIDITY <- 100 * as.numeric(IndexStand)

      IndexLogReturns <- merge(
        IndexLogReturns[, 1],
        IndexLogReturns[, 2],
        IndexLogReturns[, 3],
        IndexLogReturns[, 4],
        IndexLogReturns[, 5],
        IndexLogReturns[, 6],
        LIQUIDITY,
        all = T
      )

      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), c("Term Structure", "Credit AA", "Credit BBB", "Credit HY", "Volatility", "FXCarry", "Liquidity"))

      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate

      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 1 + NCOL(IndexLogReturns))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 1 + NCOL(IndexLogReturns), 4)
      RF <- NA
      M <- NA
    } else if (Model == "AGS_CHF_Bonds") {
      # JPM_GBI, Treasury 20+ , Treasury 1-3m , AA , BBB, HY
      # ListIndexNr <- c(818,819,820,547,1702)
      # 821: AGS Swiss Term; 822: AGS Swiss Credit AA; 823: AGS Swiss Credit A; 1702: Volatility
      ListIndexNr <- c(821, 822, 823, 1702)

      # Bei den anderen Standardfaktormodellen werden Start und EndDate im Grafikfile berechnet
      CommonPeriod <- PPCcalcGemeinsamePeriode(MandatNr, ListIndexNr, NA, StartDate, EndDate)
      StartDate <- CommonPeriod$StartDate
      EndDate <- CommonPeriod$EndDate

      MandatLogReturns <- PPCReturnsStetig(MandatNr, StartDate, EndDate)$LogOutperformance
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }

      IndexReturns <- PPCgetIndexReturns(ListIndexNr, "CHF", StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen

      # off, on
      # Indizes_Liq <- c(748,749)
      # IndexStand <- PPCgetIndexStand(Indizes_Liq, PPCGetDateOffset(StartDate,-1,0), EndDate)$IndexStand
      # LIQUIDITY <- 100*(as.numeric((IndexStand[1:(NROW(IndexStand)-1),1]-IndexStand[1:(NROW(IndexStand)-1),2])) - as.numeric((IndexStand[2:NROW(IndexStand),1]-IndexStand[2:NROW(IndexStand),2])))
      Indizes_Liq <- c(817)
      IndexStand <- PPCgetIndexStand(Indizes_Liq, PPCGetDateOffset(StartDate, 0, 0), EndDate, silent = TRUE)$IndexStand

      LIQUIDITY <- 100 * as.numeric(IndexStand)

      IndexLogReturns <- merge(
        IndexLogReturns[, 1],
        IndexLogReturns[, 2],
        CREDITA <- IndexLogReturns[, 3],
        IndexLogReturns[, 4],
        LIQUIDITY,
        all = T
      )

      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), c("Term Structure", "Credit AA", "Credit A", "Volatility", "Liquidity"))

      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate

      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 1 + NCOL(IndexLogReturns))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 1 + NCOL(IndexLogReturns), 4)
      RF <- NA
      M <- NA
    } else if (Model == "AGS_Equity_EMMA") {
      # ListIndexNr <- c(24,22,762,764,750,1702) #EM Equity
      # 993: AGS EmMa Value/Growth; 994: AGS EmMa Small/Large; 1027: Global Momentum; 1702: Volatility
      ListIndexNr <- c(993, 994, 1027, 1702) # EM Equity

      # Bei den anderen Standardfaktormodellen werden Start und EndDate im Grafikfile berechnet
      CommonPeriod <- PPCcalcGemeinsamePeriode(MandatNr, ListIndexNr, NA, StartDate, EndDate)
      StartDate <- CommonPeriod$StartDate
      EndDate <- CommonPeriod$EndDate

      MandatLogReturns <- PPCReturnsStetig(MandatNr, StartDate, EndDate)$LogOutperformance
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }

      IndexReturns <- PPCgetIndexReturns(ListIndexNr, "CHF", StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen

      IndexLogReturns <- merge(
        IndexLogReturns[, 1],
        IndexLogReturns[, 2],
        IndexLogReturns[, 3],
        IndexLogReturns[, 4],
        all = T
      )

      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), c("Value/Growth", "Small/Large", "Momentum", "Volatility"))

      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate

      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 1 + NCOL(IndexLogReturns))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 1 + NCOL(IndexLogReturns), 4)
      RF <- NA
      M <- NA
    } else if (Model == "AGS_flexibel") {
      if (all(is.na(ListIndexNr[1]))) {
        return(NA)
      }
      # Bei den anderen Standardfaktormodellen werden Start und EndDate im Grafikfile berechnet
      CommonPeriod <- PPCcalcGemeinsamePeriode(MandatNr, ListIndexNr, NA, StartDate, EndDate)
      StartDate <- CommonPeriod$StartDate
      EndDate <- CommonPeriod$EndDate

      MandatLogReturns <- PPCReturnsStetig(MandatNr, StartDate, EndDate)$LogOutperformance
      if (all(is.na(MandatLogReturns))) {
        return(NA)
      }

      # 01.03.2017	HHU	falls Mandat nicht in CHF, müssen alle Indizes ausser Faktorindizes in der Mandatswährung geladen werden
      # 01.03.2017	HHU	im GO werden die Faktorindizes neu nicht mehr in die verschiedenen Währungen umgerechnet, der Initialwert wird in allen Spalten kopiert.
      m_currency <- PPCgetMandateDetails(MandatNr)$Currency

      IndexReturns <- PPCgetIndexReturns(ListIndexNr, m_currency, StartDate, EndDate)
      if (all(is.na(IndexReturns))) {
        return(NA)
      }
      IndexLogReturns <- IndexReturns$LogRenditen

      Date <- zoo::index(IndexReturns$Renditen)
      cNames <- c(as.character(Bezeichnung[579, Language + 1]), IndexReturns$IndexName)

      StartDatePlot <- IndexReturns$StartDate
      EndDatePlot <- IndexReturns$EndDate

      # Modell rechnen
      End <- length(MandatLogReturns)
      loops <- End - Rollperiod + 1
      FactorOutput <- matrix(, loops, 1 + NCOL(IndexLogReturns))
      colnames(FactorOutput) <- cNames
      Statistics <- matrix(, 1 + NCOL(IndexLogReturns), 4)
      RF <- NA
      M <- NA
    }
  }

  if (loops < 1) {
    return(NA)
  }

  for (i in 1:(loops)) {
    apt <- lm((MandatLogReturns[i:((End - loops) + i)]) ~ IndexLogReturns[i:((End - loops) + i)])
    FactorOutput[i, ] <- apt$coef
  }

  # Regression über den gesamten Zeitraum
  apt <- lm(MandatLogReturns ~ IndexLogReturns)
  lm_summary <- summary(apt)
  Adj_R_Squared <- lm_summary$adj.r.squared
  DF <- lm_summary$df[2]
  sigma <- lm_summary$sigma
  Statistics <- lm_summary$coefficients
  rownames(Statistics) <- colnames(FactorOutput)
  # 10.11.2016 HHU	Schätzer soll Koeffizienten heissen.
  colnames(Statistics) <- c(
    as.character(Bezeichnung[572, Language + 1]), as.character(Bezeichnung[573, Language + 1]),
    as.character(Bezeichnung[574, Language + 1]), as.character(Bezeichnung[575, Language + 1])
  )

  # Alpha annualisieren
  FactorOutput[, 1] <- FactorOutput[, 1] * 12
  Statistics[1, c(1:2)] <- Statistics[1, c(1:2)] * 12

  # modell jensen oder FF -> h0: markt-beta = 1
  if (Model == "Jensen" | Model == "FF" | Model == "Carhart" | Model == "Barra") {
    Statistics[2, 3] <- (Statistics[2, 1] - 1) / Statistics[2, 2]
    Statistics[2, 4] <- (1 - pt(abs(Statistics[2, 3]), lm_summary$df[2])) * 2
  }

  FactorOutput <- zoo::zoo(FactorOutput, order.by = Date[(Rollperiod):NROW(Date)])

  if (Model == "Index2Index") {
    Name <- PPCgetIndexName(MandatNr)$IndexName
    AM <- ""
  } else {
    Name <- PPCgetMandateDetails(MandatNr)$Produktname
    AM <- PPCgetMandateDetails(MandatNr)$AMKuerzel
  }

  return(list(
    FactorOutput = FactorOutput,
    Adj_R_Squared = Adj_R_Squared,
    sigma = sigma * sqrt(12),
    DF = DF,
    Statistics = Statistics,
    StartDatePlot = StartDatePlot,
    EndDatePlot = EndDatePlot,
    DateVector = Date,
    MandatName = Name, # PPCgetMandateDetails(MandatNr)$Produktname,
    AM = AM,
    RF = RF,
    M = M
  ))
}

