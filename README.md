# ma-networks
This repository contains analysis code for this project on the Open Science Framework: https://osf.io/w63br.

# Data

## Clean

- Has 807 participants (no test accounts)

## Set A

- **General**
  - Has more tables than Set B (including those, e.g., `participant`, likely from other server)
  - Has some 344-character `participantRSA` values, some of which are in `R34_cleaning_script.R`
  - Is more similar to tables loaded and described in `R34.ipynb`
    - Has similar file names and same names of participant ID columns
    - But has fewer participants (so data are different), except for `demographic`
    and `task_log` tables (which have same number of participants)
<details>
<summary>Counts</summary>

```
> set_a_vs_r34.ipynb_Ns
      bbsiq   dass21_as   dass21_ds demographic          oa participant          rr    task_log 
         -1         -61          -1           0         -44         -61          -1           0
```
</details>

- **Participant table**
  - Missing 36 participants in clean data listed as excluded from original analyses
  due to server error
    - These participants are also missing in DASS-21-AS and OA tables in Set A but 
    have DASS-21-AS and OA data in Set B
      - And their baseline DASS-21-AS and OA data in Set B matches that in `R34_Cronbach.csv`
      file on main outcomes paper OSF project
    - These participants have the same number of rows in other tables in Sets A and B
- **Task Log table**
  - Has discrepancies between `session` and date-related values in this table versus 
  `session` and `date` in other tables in both Sets A and B
- **DASS-21-AS table**
  - Has `sessionId`, and session column at screening has both `ELIGIBLE` and blank values
- **OASIS table**
  - TODO
- **RR table**
  - Matches values in clean data
- **BBSIQ table**
  - Matches values in clean data

## Set B

- **General**
  - Has no 344-character `participantRSA` values
  - Is less similar to tables loaded and described in `R34.ipynb`
    - Has different file names and some different names of participant ID columns
    - Has far fewer participants
<details>
<summary>Counts</summary>

```
> set_b_vs_r34.ipynb_Ns
      bbsiq   dass21_as   dass21_ds demographic          oa          rr 
       -436        -434        -435        -436        -434        -429
```
</details>

- **No participant table**
- **DASS-21-AS table**
  - Session column at screening has only `ELIGIBLE` values
  - 58 participants (not in Set A and including all 36 listed as excluded from original 
  analyses due to server error but now in clean data) have dates that are 11:14 characters
- **OA table**
  - 42 participants (not in Set A and including all 36 noted above) have dates that are 
  11:14 characters
- **RR table**
  - Matches values in clean data
- **BBSIQ table**
  - Matches values in clean data

# Code

TODO