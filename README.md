# R.FXN.Slopes

Modelling the impact of Frataxin Levels on Disease Progression in FA

code.DM

- DM.FXN.0*

  - does all the "dirty work"" about individual datasets

- DM.FXN.1.combine

  - combines the above (including TRACKFA)
  - aligns and fixes individual patient issues in FXN data
  
- DM.FXN.2.post.process

  - decide on FXN datasets do include
  - defines analysis sets for FXN
 
- DM.FXN.3.add.clinical.data

  - adds slope datasets, using .dd('fars.slope') ( datasets and procedures defined in R.DM )
  
- all the above datasets call the earlier script first. 