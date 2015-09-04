This project recreates Travis Andrews' "Empirical Successional Mapping." The purpose was to make it work on a previously generated PSQL database of FIA data (which is easy to update and quick to access), and to simply get familiar with and somewhat clean up Travis' code. That said, the code is conceptually almost all his work. The ESM data can then be translated into inputs for the ED model, with the goal of seeing whether ED is able to reproduce empirical demographic patterns. 

The scripts still reference data files I received from Travis, which are too big to include in this repository. However, they're only used for comparing to my own version of the ESM—they're not needed to reproduce the generation of the ESM from the FIA database.

Obviously, the scripts do need access to an FIA database. There's one on the BU server, or you can see my fia_psql project for more infor on creating one:

  https://github.com/ryankelly-uiuc/fia_psql

Note also that the first script relies on 'PSQL_utils.R', a standalone version of some of PEcAn's DB tools. It would probably work to just load PEcAn library instead, if you have it. Otherwise, the 'PSQL_utils.R' script can be obtained from my the fia_psql repository mentioned above (fia_psql/Scripts/Distribute/PSQL_utils.R).


Contents:
- From_T.Andrews
  - Some files from Travis, including a species-code-to-PFT mapping table, and a draft of the ESM manuscript. Would also contain raw ESM data he sent me, but as noted above the files are too big for GitHub, and were only needed for sanity-checking my own ESM version the first time around.
  
- Scripts
  - Sequential sqripts for recreating the ESM from FIA data, converting it to ED initial conditions, and then running corresponding ED simulations. 