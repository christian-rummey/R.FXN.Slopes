# R.FXN.Slopes

Modelling the impact of Frataxin Levels on Disease Progression in FA

# code.DM

-   DM.FXN.0\*.R

    -   does all the "dirty work"" about individual FXN datasets
    -   these are individually saved in /DATA derived

-   DM.FXN.1.combine.R

    -   combines the above from DATA derived (including TRACKFA)
    -   aligns and fixes individual patient issues in FXN data

-   DM.FXN.2.post.process.R

    -   calls DM.FXN.1.combine.R
    -   decides on FXN datasets do include
    -   defines analysis sets for FXN

-   DM.FXN.3.add.clinical.data.R

    -   calls DM.FXN.2.post.process.R
    -   adds slope datasets, using .dd('fars.slope') ( datasets and procedures defined in R.DM )

# code.Describe Demo Table Programs

# code.Describe Dataset pre-Analyses work

# code.Describe LoA / DM TTE 

# FXN Slope Modelling

-   FXN Slope Modelling

    -   
