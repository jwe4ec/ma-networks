# ma-networks
This repository contains analysis code for this project on the Open Science Framework: https://osf.io/w63br.

***TODO: Reconcile differences between clean data and Set A with added data***
***TODO: Recode session to be consecutive in OASIS table for Set A with added data***
***TODO: Update README***





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
    oa                      2574  2592  -18
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
  - Matches values in clean data after handling multiple entries for 41 participants 
  (all of which in Set A were missing a session and had two entries for another session)
    - Rather than keeping most recent entry, this was done by sorting each participant's 
    OASIS entries chronologically and then recoding per the expected session order for the 
    number of entries present
    - (e.g., [MT-Data-ManagingAnxietyStudy](https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy)
    Issues [1](https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy/issues/1#issue-403285089)
    and [2](https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy/issues/2#issue-403285690))
  - But 111 others without multiple entries have one session skipped in OASIS table 
  (and in clean OASIS data)
    - 109 have S1 skipped, and 2 have S3 skipped
    - As a result, session dates in OASIS table are inconsistent with those in 
    RR table for 12 participants
      - (18 other participants have different [but not inconsistent] dates for other reasons)
    - Sessions in OASIS table differ from those for OASIS entries in Task Log table,
    unless sessions in OASIS table are recoded to be consecutive
      - In Set B, the session values in OASIS table seem to have been recoded to be consecutive
    - Seems implausible that, per sessions in OASIS table, in many cases S2 OASIS 
    was completed a few min after pretx OASIS. Seems more plausible that S1 OASIS 
    was completed a few min after pretx OASIS.
      - (Participants could start S1 right after pretx but had to wait 2 days after
      completing S1 to start S2)
    - ***TODO: Seems we should recode sessions in Set A OASIS table to be consecutive.
      Asked Julie/Sonia/Laura/Bethany about this on 9/15/2025.***
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
  - No participants have skipped sessions in OASIS table
    - As a result, after restricting to shared participants in clean OASIS data, 
    total scores seem discrepant for 24 participants. However, in each case the 
    total scores are the same, but sessions are mismatched.
      - All these participants are in the 109 participants in Set A with skipped 
      S1 in OASIS table. Clean OASIS data also skips S1 (lists S2 instead), whereas 
      Set B OASIS table lists consecutive sessions.
    - Unlike in Set A, session dates in OASIS table are consistent with those in RR table,
    and sessions in Set B OASIS table are identical to those for OASIS entries in 
    Set A Task Log table

- **RR table**
  - Matches values in clean data
- **BBSIQ table**
  - Matches values in clean data

# Code

TODO