# ma-networks
This repository contains analysis code for this project on the Open Science Framework: https://osf.io/w63br.

# Data

## Clean

- **General**
  - Has 807 participants (no test accounts)
  - May be some issues with how `R34.ipynb` handled multiple entries (e.g., may have
  sorted entries lexicographically vs. chronologically)
- **OASIS data**
  - `R34.ipynb` seems to compute total score by treating item-level responses of `NA` as 0
- **BBSIQ data**
  - `R34.ipynb` seems to compute negative bias score as a mean of ratios of means
  (as now described in wiki of OSF project for main outcomes paper)

## Set A

- **General**
  - Has more tables than Set B (including those, e.g., `participant`, likely from other server)
  - Has some 344-character `participantRSA` values, some of which are in `R34_cleaning_script.R`
    - Some values remain after correcting the known values
  - Is more similar to tables loaded and described in `R34.ipynb`
    - Has similar file names and same names of participant ID columns
    - But has fewer participants (so data are different), except for `demographic`
    and `task_log` tables (which have same number of participants):
      ```text
      > set_a_vs_r34.ipynb_Ns
      bbsiq   dass21_as   dass21_ds demographic          oa participant          rr    task_log 
         -1         -61          -1           0         -44         -61          -1           0
      ```
  - Has unexpected multiple entries in `dass21_as` (all are multiple screening attempts), 
  `oa`, and `task_log` (involving both `suds` and other tasks) tables
  - After restricting to shared participants in each table, Set A has more rows
  in some tables and fewer rows in others (especially for `oa`) than clean data:
    ```text
    > set_a_vs_cln_nrow
                           set_a clean diff
    bbsiq                   1233  1229    4
    dass21_as                871   875   -4
    dass21_ds               1175  1174    1
    demographic              807   807    0
    oa                      2533  2592  -59
    participant_export_dao   771   771    0
    rr                      1186  1183    3
    ```
- **Participant table**
  - Missing 36 participants in clean data listed as excluded from original analyses
  due to server error
    - Also missing in DASS-21-AS and OA tables in Set A but have DASS-21-AS and OA data in Set B
      - And their baseline DASS-21-AS and OA data in Set B matches that in `R34_Cronbach.csv`
      file on main outcomes paper OSF project
    - These participants have the same number of rows in other tables in Sets A and B
  - Otherwise, CBM condition is same as in clean data
- **Task Log table**
  - Has `corrected_datetime` in addition to `date`
  - Has discrepancies between `session` and date-related values (inc. `corrected_datetime`)
  in this table versus `session` and `date` in other tables in both Sets A and B
    - For at least one example participant (623), Set B and clean data have same sessions
- **DASS-21-AS table**
  - Has `sessionId`, and session column at screening has both `ELIGIBLE` and blank values
- **OASIS table**
  - After restricting to shared participants in clean OASIS data, total scores are
  discrepant for 19 participants, all of which in Set A are missing a session and
  have two entries for another session
    - And participants who happen to have same total scores still have missing and
    multiple entries in Set A
    - Seems due to potential recoding of session to resolve multiple (and missing) 
    entries in clean data (vs. keeping most recent), and doesn't seem merely 
    due to potential lexicographical sorting by `R34.ipynb`
      - TODO: Does clean data show any skipped sessions?
      - TODO: Was `task_log` used to correct the session values?
    - When excluding all 41 participants with multiple entries in OASIS table, all
    total scores in Set A are the same as those in clean data
  - Session dates in OASIS table are inconsistent with those in RR table, even
  though the RR data matches values in clean data
    - Suggests that session values in OASIS table may be wrong (and indeed, they 
    don't match those in clean data)
- **RR table**
  - Matches values in clean data
- **BBSIQ table**
  - Matches values in clean data

## Set B

- **General**
  - Has no 344-character `participantRSA` values
  - Is less similar to tables loaded and described in `R34.ipynb`
    - Has different file names and some different names of participant ID columns
    - Has far fewer participants:
      ```text
      > set_b_vs_r34.ipynb_Ns
      bbsiq   dass21_as   dass21_ds demographic          oa          rr 
       -436        -434        -435        -436        -434        -429
      ```
  - Has no unexpected multiple entries
  - After restricting to shared participants in each table, Set B has more rows in some 
  tables and fewer rows in others (especially for `dass21_as`) than clean data:
    ```text
    > set_b_vs_cln_nrow
                set_b clean diff
    bbsiq        1232  1229    3
    dass21_as     807   913 -106
    dass21_ds    1174  1174    0
    demographic   807   807    0
    oa           2661  2679  -18
    rr           1185  1183    2
    ```
  - Although `imagery_prime` table has `prime` condition, no table has CBM condition
    - Perhaps could be derived from `trial_dao` table by computing proportion of
    positive scenarios (see `positive` column)
- **No participant table**
- **DASS-21-AS table**
  - Session column at screening has only `ELIGIBLE` values
  - 58 participants (not in Set A and including all 36 listed as excluded from original 
  analyses due to server error but now in clean data) have dates that are 11:14 characters
- **OA table**
  - 42 participants (not in Set A and including all 36 noted above) have dates that are 
  11:14 characters
  - Seems to have corrected session values for at least some participants
    - See [MT-Data-ManagingAnxietyStudy](https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy)
    Issues [1](https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy/issues/1#issue-403285089)
    and [2](https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy/issues/2#issue-403285690)
  - After restricting to shared participants in clean OASIS data, total scores seem
  discrepant for 24 participants (different from the 19 participants discrepant in 
  Set A). However, in each case the total scores are actually the same, but sessions
  are mismatched. The session column in clean data skips Session 1 (i.e., lists Session 
  2 instead), whereas Set B lists consecutive sessions (i.e., lists Session 1).
  - Unlike in Set A, session dates in OASIS table are consistent with those in RR table
- **RR table**
  - Matches values in clean data
- **BBSIQ table**
  - Matches values in clean data

# Code

TODO