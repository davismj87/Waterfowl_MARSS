rm(list=ls(all=TRUE))
library(MARSS)
library(ggplot2)
library(gridExtra)

###LOADING DATA###
	PC = read.csv("E:\\UW PHD\\Courses\\FISH 507 Time Series\\Final_Project\\Analyses_New\\Q2_Species.csv")
	head(PC)
	Sites = c("WA89_1","WA89_2","OR69_1","OR69_2","OR69_3","CA14_1","CA14_2","CA14_3","CA14_4",
		"CA14_5","CA14_6","CA14_7", "Year","Species")
	PC_Data = PC[,(colnames(PC) %in% Sites)]
	head(PC_Data)
	PC_Data = PC_Data[,c(1,2,14,6,13,4,5,7,3,8,9,10,11,12)]
	hist(PC_Data$WA89_1)

###HYPOTHESES###

		H1 = factor(rep("Pan",12))
		H2 = factor(c("N","N","N","N","N","N","N","S","S","S","S","S"))
		H3 = factor(c("C","I","C","I","I","C","I","I","C","I","C","I"))
		H4 = factor(c("NC","NI","NC","NI","NI","NC","NI","SI","SC","SI","SC","SI"))
		H5 = factor(1:12)	



###MALLARD###

	#Setting up the data#
		MALL = subset(PC_Data, Species == "MALL")
		years = MALL[,"Year"]
		dat = MALL[,!(colnames(MALL) %in% c("Year"))]
		dat = dat[,!(colnames(dat) %in% c("Species"))]
		dat = t(dat) 
		colnames(dat) = years
		n = nrow(dat)-1
		head(dat)
		headers = as.vector(rownames(dat))

	###MALLARD MODEL TESTING###

		#SAME PROCESS ERROR (Q), SAME OBSERVATION ERROR (R), NO DRIFT (U)

			#PANMICTIC#
			Fit.M1.H1 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M1.H2 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M1.H3 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M1.H4 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M1.H5 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M1.AICc = c(Fit.M1.H1$AICc,Fit.M1.H2$AICc,Fit.M1.H3$AICc,Fit.M1.H4$AICc,Fit.M1.H5$AICc)
			M1.AICc

		#UNIQUE PROCESS ERROR (Q), SAME OBSERVATION ERROR (R), NO DRIFT (U)

			#PANMICTIC#
			Fit.M2.H1 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M2.H2 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M2.H3 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M2.H4 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M2.H5 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M2.AICc = c(Fit.M2.H1$AICc, Fit.M2.H2$AICc, Fit.M2.H3$AICc, Fit.M2.H4$AICc, Fit.M2.H5$AICc)
			M2.AICc

		#UNIQUE PROCESS ERROR W CORRELATION (Q), SAME OBSERVATION ERROR (R), NO DRIFT (U)

			#PANMICTIC#
			Fit.M3.H1 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M3.H2 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M3.H3 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q ="unconstrained", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M3.H4 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M3.H5 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=3000))

			M3.AICc = c(Fit.M3.H1$AICc, Fit.M3.H2$AICc, Fit.M3.H3$AICc, Fit.M3.H4$AICc, Fit.M3.H5$AICc)
			M3.AICc

		#SAME PROCESS ERROR (Q), UNIQUE OBSERVATION ERROR (R), NO DRIFT (U)

			#PANMICTIC#
			Fit.M4.H1 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M4.H2 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M4.H3 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M4.H4 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M4.H5 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M4.AICc = c(Fit.M4.H1$AICc,Fit.M4.H2$AICc,Fit.M4.H3$AICc,Fit.M4.H4$AICc,Fit.M4.H5$AICc)
			M4.AICc

		#UNIQUE PROCESS ERROR (Q), UNIQUE OBSERVATION ERROR (R), NO DRIFT (U)

			#PANMICTIC#
			Fit.M5.H1 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M5.H2 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M5.H3 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M5.H4 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M5.H5 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "diagonal and unequal", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M5.AICc = c(Fit.M5.H1$AICc, Fit.M5.H2$AICc, Fit.M5.H3$AICc, Fit.M5.H4$AICc, Fit.M5.H5$AICc)
			M5.AICc

		#UNIQUE PROCESS ERROR W CORRELATION (Q), UNIQUE OBSERVATION ERROR (R), NO DRIFT (U)

			#PANMICTIC#
			Fit.M6.H1 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M6.H2 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M6.H3 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q ="unconstrained", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M6.H4 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M6.H5 = MARSS(dat, 
				model=list(B = "identity", U = "zero", Q = "unconstrained", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M6.AICc = c(Fit.M6.H1$AICc, Fit.M6.H2$AICc, Fit.M6.H3$AICc, Fit.M6.H4$AICc, Fit.M6.H5$AICc)
			M6.AICc

		#SAME PROCESS ERROR (Q), SAME OBSERVATION ERROR (R), SAME DRIFT (U)

			#PANMICTIC#
			Fit.M7.H1 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M7.H2 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M7.H3 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M7.H4 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M7.H5 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M7.AICc = c(Fit.M7.H1$AICc,Fit.M7.H2$AICc,Fit.M7.H3$AICc,Fit.M7.H4$AICc,Fit.M7.H5$AICc)
			M7.AICc

		#UNIQUE PROCESS ERROR (Q), SAME OBSERVATION ERROR (R), SAME DRIFT (U)

			#PANMICTIC#
			Fit.M8.H1 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M8.H2 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M8.H3 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M8.H4 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M8.H5 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M8.AICc = c(Fit.M8.H1$AICc, Fit.M8.H2$AICc, Fit.M8.H3$AICc, Fit.M8.H4$AICc, Fit.M8.H5$AICc)
			M8.AICc

		#UNIQUE PROCESS ERROR W CORRELATION (Q), SAME OBSERVATION ERROR (R), SAME DRIFT (U)

			#PANMICTIC#
			Fit.M9.H1 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M9.H2 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M9.H3 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q ="unconstrained", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M9.H4 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M9.H5 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M9.AICc = c(Fit.M9.H1$AICc, Fit.M9.H2$AICc, Fit.M9.H3$AICc, Fit.M9.H4$AICc, Fit.M9.H5$AICc)
			M9.AICc

		#SAME PROCESS ERROR (Q), UNIQUE OBSERVATION ERROR (R), SAME DRIFT (U)

			#PANMICTIC#
			Fit.M10.H1 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M10.H2 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M10.H3 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M10.H4 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M10.H5 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M10.AICc = c(Fit.M10.H1$AICc,Fit.M10.H2$AICc,Fit.M10.H3$AICc,Fit.M10.H4$AICc,Fit.M10.H5$AICc)
			M10.AICc

		#UNIQUE PROCESS ERROR (Q), UNIQUE OBSERVATION ERROR (R), SAME DRIFT (U)

			#PANMICTIC#
			Fit.M11.H1 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M11.H2 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M11.H3 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M11.H4 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M11.H5 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "diagonal and unequal", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=3000))

			M11.AICc = c(Fit.M11.H1$AICc, Fit.M11.H2$AICc, Fit.M11.H3$AICc, Fit.M11.H4$AICc, Fit.M11.H5$AICc)
			M11.AICc

		#UNIQUE PROCESS ERROR W CORRELATION (Q), UNIQUE OBSERVATION ERROR (R), SAME DRIFT (U)

			#PANMICTIC#
			Fit.M12.H1 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M12.H2 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M12.H3 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q ="unconstrained", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M12.H4 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M12.H5 = MARSS(dat, 
				model=list(B = "identity", U = "equal", Q = "unconstrained", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M12.AICc = c(Fit.M12.H1$AICc, Fit.M12.H2$AICc, Fit.M12.H3$AICc, Fit.M12.H4$AICc, Fit.M12.H5$AICc)
			M12.AICc

		#SAME PROCESS ERROR (Q), SAME OBSERVATION ERROR (R), UNIQUE DRIFT (U)

			#PANMICTIC#
			Fit.M13.H1 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M13.H2 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M13.H3 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M13.H4 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M13.H5 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M13.AICc = c(Fit.M13.H1$AICc,Fit.M13.H2$AICc,Fit.M13.H3$AICc,Fit.M13.H4$AICc,Fit.M13.H5$AICc)
			M13.AICc

		#UNIQUE PROCESS ERROR (Q), SAME OBSERVATION ERROR (R), UNIQUE DRIFT (U)

			#PANMICTIC#
			Fit.M14.H1 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M14.H2 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M14.H3 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M14.H4 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M14.H5 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M14.AICc = c(Fit.M14.H1$AICc, Fit.M14.H2$AICc, Fit.M14.H3$AICc, Fit.M14.H4$AICc, Fit.M14.H5$AICc)
			M14.AICc

		#UNIQUE PROCESS ERROR W CORRELATION (Q), SAME OBSERVATION ERROR (R), UNIQUE DRIFT (U)

			#PANMICTIC#
			Fit.M15.H1 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H1, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M15.H2 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H2, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M15.H3 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q ="unconstrained", A = "scaling",  
				Z = H3, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M15.H4 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H4, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M15.H5 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H5, R = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M15.AICc = c(Fit.M15.H1$AICc, Fit.M15.H2$AICc, Fit.M15.H3$AICc, Fit.M15.H4$AICc, Fit.M15.H5$AICc)
			M15.AICc

		#SAME PROCESS ERROR (Q), UNIQUE OBSERVATION ERROR (R), UNIQUE DRIFT (U)

			#PANMICTIC#
			Fit.M16.H1 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M16.H2 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M16.H3 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M16.H4 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M16.H5 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M16.AICc = c(Fit.M16.H1$AICc,Fit.M16.H2$AICc,Fit.M16.H3$AICc,Fit.M16.H4$AICc,Fit.M16.H5$AICc)
			M16.AICc

		#UNIQUE PROCESS ERROR (Q), UNIQUE OBSERVATION ERROR (R), UNIQUE DRIFT (U)

			#PANMICTIC#
			Fit.M17.H1 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M17.H2 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M17.H3 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M17.H4 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M17.H5 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "diagonal and unequal", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			M17.AICc = c(Fit.M17.H1$AICc, Fit.M17.H2$AICc, Fit.M17.H3$AICc, Fit.M17.H4$AICc, Fit.M17.H5$AICc)
			M17.AICc

		#UNIQUE PROCESS ERROR W CORRELATION (Q), UNIQUE OBSERVATION ERROR (R), UNIQUE DRIFT (U)

			#PANMICTIC#
			Fit.M18.H1 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H1, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#NORTH/SOUTH#
			Fit.M18.H2 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H2, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#COAST/INLAND#
			Fit.M18.H3 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q ="unconstrained", A = "scaling",  
				Z = H3, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#REGIONAL#
			Fit.M18.H4 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H4, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNIT#
			Fit.M18.H5 = MARSS(dat, 
				model=list(B = "identity", U = "unequal", Q = "unconstrained", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000, conv.test.slope.tol=500))

			M18.AICc = c(Fit.M18.H1$AICc, Fit.M18.H2$AICc, Fit.M18.H3$AICc, Fit.M18.H4$AICc, Fit.M18.H5$AICc)
			M18.AICc

	###PLOTTING DATA###

	Fit.M4.H5$states
	
	par(mfrow=(c(3,4)))
	for(i in 1:12){
		plot(years, dat[i,], pch=19, ylim=c(0,14), ylab="log(abundance)", xlab="", main=headers[i])
		lines(years, Fit.M4.H5$states[i,], lwd=1, col="black")
		lines(years, Fit.M4.H5$states[i,] + 1.96*Fit.M4.H5$states.se[i,], lwd=1, col="red", lty=2)
		lines(years, Fit.M4.H5$states[i,] - 1.96*Fit.M4.H5$states.se[i,], lwd=1, col="red", lty=2)
	}

	par(mfrow=(c(3,4)))
	for(i in 1:12){
		acf(residuals(Fit.M4.H5)$model.residuals[i,], main=headers[i])
	}

	resids = residuals(Fit.M4.H5)
	par(mfrow=(c(3,4)))
	for(i in 1:12){
		plot(resids$model.residuals[i,], ylab="model residuals", xlab="")
		abline(h=0)
		title(rownames(dat)[i])
	}


	###SIMULATED TRAJECTORIES###

	#Compare trajectories for two best-fit models#
	Fit.M4.H5.Sim = MARSSsimulate(Fit.M4.H5, tSteps = 50)
	Fit.M10.H5.Sim = MARSSsimulate(Fit.M10.H5, tSteps = 50)
		par(mfrow=(c(2,1)))
		plot(Fit.M4.H5.Sim$sim.states, type="p")
		plot(Fit.M10.H5.Sim$sim.states, type="p")

	#Bootstrap output#
	Bootstrap = matrix(0,1000,12)
	Inits = matrix(0,1000,12)
	for(i in 1:1000){
		for(j in 1:12){
			Fit.M10.H5.Sim = MARSSsimulate(Fit.M10.H5, tSteps = 50)
			sim.output = as.data.frame(Fit.M10.H5.Sim$sim.states)
			Bootstrap[i,j] = sim.output[j,50]
			Inits[i,j] = sim.output[j,1]
			}
		}

	Quasi = Bootstrap < 0.2*Inits
	100*colSums(Quasi == "TRUE")/1000
	Decline = 100*(Bootstrap - Inits)/Inits
	colMeans(Decline)


	###COVARIATES###

	w_dat = dat[,26:56]

	Mean_Temp = read.csv("E:\\UW PHD\\Courses\\FISH 507 Time Series\\Final_Project\\Weather_New\\Mean_Temp.csv")
		years = Mean_Temp[,"Year"]
		W_Sites = c("WA89_1","WA89_2","OR69_1","OR69_2","OR69_3","CA14_1","CA14_2","CA14_3","CA14_4",
		"CA14_5","CA14_6","CA14_7")
		Mean_Temp = Mean_Temp[,!(colnames(Mean_Temp) %in% c("Year"))]
		colnames(Mean_Temp) = W_Sites
		Mean_Temp = t(Mean_Temp)
		colnames(Mean_Temp) = years	

	Max_Temp = read.csv("E:\\UW PHD\\Courses\\FISH 507 Time Series\\Final_Project\\Weather_New\\Max_Temp.csv")
		Max_Temp = Max_Temp[,!(colnames(Max_Temp) %in% c("Year"))]
		colnames(Max_Temp) = W_Sites
		Max_Temp = t(Max_Temp)
		colnames(Max_Temp) = years

	Min_Temp = read.csv("E:\\UW PHD\\Courses\\FISH 507 Time Series\\Final_Project\\Weather_New\\Min_Temp.csv")
		Min_Temp = Min_Temp[,!(colnames(Min_Temp) %in% c("Year"))]
		colnames(Min_Temp) = W_Sites
		Min_Temp = t(Min_Temp)
		colnames(Min_Temp) = years
	
	Precip = read.csv("E:\\UW PHD\\Courses\\FISH 507 Time Series\\Final_Project\\Weather_New\\Precip.csv")
		Precip = Precip[,!(colnames(Precip) %in% c("Year"))]
		colnames(Precip) = W_Sites
		Precip = t(Precip)
		colnames(Precip) = years

	Snowfall = read.csv("E:\\UW PHD\\Courses\\FISH 507 Time Series\\Final_Project\\Weather_New\\Snowfall.csv")
		Snowfall = Snowfall[,!(colnames(Snowfall) %in% c("Year"))]
		colnames(Snowfall) = W_Sites
		Snowfall = t(Snowfall)
		colnames(Snowfall) = years

		#SAME PROCESS ERROR (Q), UNIQUE OBSERVATION ERROR (R), NO DRIFT (U)

			#NO COVARIATES (D)

			Fit.M19.H1 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#EQUAL MEAN TEMP
			Fit.M19.H2 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Mean_Temp,
				D = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNEQUAL MEAN TEMP
			Fit.M19.H3 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Mean_Temp,
				D = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#EQUAL MINTEMP
			Fit.M19.H4 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Min_Temp,
				D = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNEQUAL MINTEMP
			Fit.M19.H5 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Min_Temp,
				D = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#EQUAL MAXTEMP
			Fit.M19.H6 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Max_Temp,
				D = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNEQUAL MAXTEMP
			Fit.M19.H7 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Max_Temp,
				D = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#EQUAL PRECIP
			Fit.M19.H8 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Precip,
				D = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNEQUAL PRECIP
			Fit.M19.H9 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Precip,
				D = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#EQUAL SNOWFALL
			Fit.M19.H10 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Snowfall,
				D = "diagonal and equal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))

			#UNEQUAL SNOWFALL
			Fit.M19.H11 = MARSS(w_dat, 
				model=list(B = "identity", U = "zero", Q = "equalvarcov", A = "scaling",  
				Z = H5, R = "diagonal and unequal",
				d = Snowfall,
				D = "diagonal and unequal",
				x0 = "unequal",tinitx = 0), 
				control=list(maxit=2000))
		




