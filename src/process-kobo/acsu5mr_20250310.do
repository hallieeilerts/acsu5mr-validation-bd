
*You have to change data file and sheet name in two places (Line number-6; Line number-813)

cd "C:\Users\HEilerts\Institute of International Programs Dropbox\Hallie Eilerts-Spinelli\ACSU5MR\acsu5mr-validation-bd\data"
***********************************************
import excel "aXmwdKgwzBQDSJH6mm7jjM_2025_03_10_14_34_54.xlsx", sheet("aXmwdKgwzBQDSJH6mm7jjM") firstrow clear

*Excluding data before 24-12-2024

keep if _index>=175

***********************************************

*Append the newly downloaded data with previous dataset

append using Parent_file.dta, force

sort _index

***********************************************

*RENAME VARIABLES

rename begin_group_SazeMvXSZserial1 serial1
rename begin_group_SazeMvXSZserial2 serial2
rename begin_group_SazeMvXSZs3 s3
rename begin_group_SazeMvXSZserial3 serial3

rename Household_Informationa00 a00
rename Household_Informationa0_1 a0_1
rename Household_Informationa0 a0
rename Household_Informationa1 a1
rename Household_Informationa3 a3
rename Household_Informationa3_a a3_a
rename Household_Informationa4 a4
rename Household_Informationa4_a a4_a
rename Household_Informationa5 a5
rename Household_Informationa5_a a5_a
rename Household_Informationa6 a6
rename Household_Informationa6_a a6_a
rename Household_Informationa7 a7
rename Household_Informationa71 a71
rename Household_Informationa72 a72
rename Household_Informationa73 a73
rename Household_Informationa74 a74
rename Household_Informationa75 a75
rename Household_Informationa76 a76
rename Household_Informationa77 a77
rename Household_Informationa78 a78
rename Household_Informationa79 a79
rename Household_Informationa710 a710
rename Household_Informationa711 a711
rename Household_Informationa712 a712
rename Household_Informationa713 a713
rename Household_Informationa714 a714
rename Household_Informationa715 a715
rename Household_Informationa716 a716
rename Household_Informationa717 a717
rename Household_Informationa718 a718
rename Household_Informationa719 a719
rename Household_Informationa720 a720
rename Household_Informationa721 a721
rename Household_Informationa722 a722
rename Household_Informationa723 a723
rename Household_Informationa724 a724
rename Household_Informationa725 a725
rename Household_Informationa726 a726
rename Household_Informationa727 a727
rename Household_Informationa728 a728
rename Household_Informationa729 a729
rename Household_Informationa000 a000

rename Respondent_Informationb00 b00
rename Respondent_Informationb110_a b110_a
rename Respondent_Informationb110_b b110_b
rename Respondent_Informationb111 b111
rename Respondent_Informationb111_a b111_a
rename Respondent_Informationb112 b112
rename Respondent_Informationb112_a b112_a
rename Respondent_Informationb113 b113
rename Respondent_Informationb113_a b113_a
rename Respondent_Informationb113_b b113_b
rename Respondent_Informationb114 b114
rename Respondent_Informationb115 b115
rename Respondent_Informationb117 b117
rename Respondent_Informationb117_a b117_a
rename Respondent_Informationb119 b119
rename Respondent_Informationb120 b120
rename Respondent_Informationb121 b121
rename Respondent_Informationb122 b122
rename Respondent_Informationb123 b123
rename Respondent_Informationb123_a b123_a
rename Respondent_Informationb130 b130
rename Respondent_Informationb130_a b130_a
rename Respondent_Informationb000 b000

rename Reproduction_sectionc100 c100
rename Reproduction_sectionc201 c201
rename Reproduction_sectionc202 c202
rename Reproduction_sectionc203_a c203_a
rename Reproduction_sectionc203_aa c203_aa
rename Reproduction_sectionc203_b c203_b
rename Reproduction_sectionc203_bb c203_bb
rename Reproduction_sectionc204 c204
rename Reproduction_sectionc205_a c205_a
rename Reproduction_sectionc205_aa c205_aa
rename Reproduction_sectionc205_b c205_b
rename Reproduction_sectionc205_bb c205_bb
rename Reproduction_sectionc206 c206
rename Reproduction_sectionc207_a c207_a
rename Reproduction_sectionc207_aa c207_aa
rename Reproduction_sectionc207_b c207_b
rename Reproduction_sectionc207_bb c207_bb
rename Reproduction_sectionc208 c208
rename Reproduction_sectionc209 c209
rename Reproduction_sectionc210 c210
rename Reproduction_sectionc211_a c211_a
rename Reproduction_sectionc212 c212
rename Reproduction_sectionc212_1 c212_1
rename Reproduction_sectionc1000 c1000

rename Reproduction_sectionPregnancy_h preg_num
rename DG c230_A
rename DH c222_A
rename DI c222_B
rename DJ c2000

rename Reproduction_sectionc300 c300
rename Reproduction_sectionc232 c232
rename Reproduction_sectionc233 c233
rename Reproduction_sectionc233_a c233_a
rename Reproduction_sectionc234 c234
rename Reproduction_sectionc235_a c235_a
rename Reproduction_sectionc235_b c235_b
rename Reproduction_sectionc235B c235B
rename Reproduction_sectionc236 c236
rename Reproduction_sectionc236_a c236_a
rename Reproduction_sectionc236_b c236_b
rename Reproduction_sectionc236_c c236_c
rename Reproduction_sectionc236_d c236_d
rename Reproduction_sectionc236_e c236_e
rename Reproduction_sectionc237 c237
rename Reproduction_sectionc238 c238
rename Reproduction_sectionc238_a c238_a
rename Reproduction_sectionc239 c239
rename Reproduction_sectionc239A c239A
rename Reproduction_sectionc239A1 c239A1
rename Reproduction_sectionc239A2 c239A2
rename Reproduction_sectionc239A3 c239A3
rename Reproduction_sectionc239A4 c239A4
rename Reproduction_sectionc239A5 c239A5
rename Reproduction_sectionc239A6 c239A6
rename Reproduction_sectionc239A7 c239A7
rename Reproduction_sectionc239A8 c239A8
rename Reproduction_sectionc239A9 c239A9
rename Reproduction_sectionc239A10 c239A10
rename Reproduction_sectionc239A11 c239A11
rename Reproduction_sectionc239A12 c239A12
rename Reproduction_sectionc239A_a c239A_a
rename Reproduction_sectionc240 c240
rename Reproduction_sectionc240_a c240_a
rename Reproduction_sectionc241 c241
rename Reproduction_sectionc242 c242
rename Reproduction_sectionc242_a c242_a
rename Reproduction_sectionc243 c243
rename Reproduction_sectionc244 c244
rename Reproduction_sectionc3000 c3000

rename Interviewers_Observationd00 d00
rename Interviewers_Observationd1 d1
rename Interviewers_Observationd2 d2
rename Interviewers_Observationd3 d3
rename Interviewers_Observationd4 d4
rename Interviewers_Observationd5 d5
rename Interviewers_Observationd6 d6
rename Interviewers_Observationd7 d7
rename Interviewers_Observationd8 d8
rename Interviewers_Observationd9 d9
rename Interviewers_Observationd000 d000

***********************************************

*Labeling the variables

label variable aa "Interview start time"

label variable serial1 "1st serial"
label variable serial2 "2nd serial"
label variable s3 "3rd serial (generated)"
label variable serial3 "3rd serial (provided)"

label variable x0 "Visit time"
label variable x0_1 "1st visit date"
label variable x0_2 "2nd visit date"
*label variable x0_3 "3rd visit date"
label variable x1 "Interview result"
label variable x1_a "Interview result (others)"

label variable a00 "Section A start time"
label variable a0_1 "Respondent name"
label variable a0 "Enumerator name"
label variable a1 "HH member number"
label variable a3 "Roof material"
label variable a3_a "Roof material (others)"
label variable a4 "Wall material"
label variable a4_a "Wall material (others)"
label variable a5 "Floor material"
label variable a5_a "Floor material (others)"
label variable a6 "Drinking water source"
label variable a6_a "Drinking water source (others)"
label variable a7 "HH asset"
label variable a71 "Fishing boat"
label variable a72 "Fishing net"
label variable a73 "General boat"
label variable a74 "Shop"
label variable a75 "Rickshaw/van"
label variable a76 "Auto-rickshaw"
label variable a77 "Power tiller"
label variable a78 "Water pump"
label variable a79 "Threshing machine"
label variable a710 "Weeding machine"
label variable a711 "IPS"
label variable a712 "Computer/laptop"
label variable a713 "Generator"
label variable a714 "Solar panel"
label variable a715 "Deep tube-well"
label variable a716 "Quilt"
label variable a717 "Mattress"
label variable a718 "Chair/table"
label variable a719 "Dining table"
label variable a720 "Almirah/showcase"
label variable a721 "Sofa"
label variable a722 "Television"
label variable a723 "Radio"
label variable a724 "Mobile"
label variable a725 "Bicycle"
label variable a726 "Motor cycle"
label variable a727 "Refrigerator"
label variable a728 "Fan"
label variable a729 "Sewing machine"
label variable a000 "Section A end time"

label variable b00 "Section B start time"
label variable b110_a "Respondents birth year"
label variable b110_b "Respondents birth month"
label variable b111 "Respondents age"
label variable b111_a "Respondents marital status"
label variable b112 "Respondents health condition"
label variable b112_a "Respondents health score"
label variable b113 "Respondents education"
label variable b113_a "Education (school/madrasa)"
label variable b113_b "Madrasa type"
label variable b114 "Completed grade"
label variable b115 "Completed class"
label variable b117 "Reading capability"
label variable b117_a "Demanded language"
label variable b119 "Newspaper read"
label variable b120 "Radio hear"
label variable b121 "Television watch"
label variable b122 "Own mobile"
label variable b123 "Type mobile"
label variable b123_a "Type mobile (others)"
label variable b130 "Religion"
label variable b130_a "Religion (others)"
label variable b000 "Section B end time"

label variable c100 "Section C1 start time"
label variable c201 "Give birth (yes/no)"
label variable c202 "Children living with respondent"
label variable c203_a "Son live with respondent"
label variable c203_aa "Son living with respondent"
label variable c203_b "Daughter live with respondent"
label variable c203_bb "Daughter living with respondent"
label variable c204 "Alive children not living with respondent"
label variable c205_a "Son not live with respondent"
label variable c205_aa "Son not living with respondent"
label variable c205_b "Daughter not live with respondent"
label variable c205_bb "Daughter not living with respondent"
label variable c206 "Children born alive but died later"
label variable c207_a "Son born alive but died later"
label variable c207_aa "Son born alive and died later"
label variable c207_b "Daughter born alive but died later"
label variable c207_bb "Daughter born alive and died later"
label variable c208 "Total child born alive"
label variable c209 "Total child number accuracy"
label variable c210 "Born dead/miscarriage/abortion (yes/no)"
label variable c211_a "Born dead/miscarriage/abortion number"
label variable c212 "Born dead/miscarriage/abortion numbers"
label variable c212_1 "Total pregnancy accuracy"
label variable c1000 "Section C1 end time"

label variable preg_num "Number of total pregnancy"
label variable c230_A "Child death between 2018 & 2024"
label variable c222_A "Pregnancy after the last one"
label variable c222_B "Pregnancy serial accuracy"
label variable c2000 "Section C2 end time"

label variable c300 "Section C3 start time"
label variable c232 "Current pregnancy (yes/no)"
label variable c233 "Pregnancy duration (week)"
label variable c233_a "Pregnancy duration (month)"
label variable c234 "Wantedness of the pregnancy"
label variable c235_a "Wanted pregnancy later/ no more child"
label variable c235_b "Wanted pregnancy later/ no child"
label variable c235B "Iron/folic-acid"
label variable c236 "Last menstruation start"
label variable c236_a "Last menstruation start (days)"
label variable c236_b "Last menstruation start (weeks)"
label variable c236_c "Last menstruation start (months)"
label variable c236_d "Last menstruation start (years)"
label variable c236_e "Last menstruation start (date)"
label variable c237 "Last menstruation within 1 year"
label variable c238 "Menstrual material"
label variable c238_a "Menstrual material (others)"
label variable c239 "Privacy during menstruation"
label variable c239A "Work restriction"
label variable c239A1 "Work restriction 1"
label variable c239A2 "Work restriction 2"
label variable c239A3 "Work restriction 3"
label variable c239A4 "Work restriction 4"
label variable c239A5 "Work restriction 5"
label variable c239A6 "Work restriction 6"
label variable c239A7 "Work restriction 7"
label variable c239A8 "Work restriction 8"
label variable c239A9 "Work restriction 9"
label variable c239A10 "Work restriction 10"
label variable c239A11 "Work restriction 11"
label variable c239A12 "Work restriction 12"
label variable c239A_a "Work restriction (others)"
label variable c240 "Remember 1st menstruation age"
label variable c240_a "Age at 1st menstruation"
label variable c241 "Pregnancy possibility within 2 cycle"
label variable c242 "Pregnancy possibility time"
label variable c242_a "Pregnancy possibility time (others)"
label variable c243 "Knowledge about pregnancy"
label variable c244 "Presence of others during section C"
label variable c3000 "Section C2 end time"

label variable d00 "Section D start time"
label variable d1 "Interview location"
label variable d2 "Interview interruption (yes/no)"
label variable d3 "Interview interruption time"
label variable d4 "Familiarity with respondent"
label variable d5 "Familiarity with other member (yes/no)"
label variable d6 "How much familiar with other member"
label variable d7 "Respondents cooperation"
label variable d8 "Anyone else around"
label variable d9 "Comments about the interview"
label variable d000 "Section D end time"

label variable dd "Interview end time"
***********************************************

*Respondent's HH visiting number

destring x0, replace
label def x0 1"1st visit" 2"2nd visit" 3"3rd visit"
label val x0 x0

*Interview status

destring x1, replace
label def x1 1"Completed" 2"Absent" 3"Migration" 4"Postponed" 5"Refused" 6"Incapacity" 7"Deceased" 8"Other"
label val x1 x1

***********************************************

*Section A: Respondents Information

*Interviewer wise data number

destring a0, replace
label def a0 1"Mofazzal hossain" 2"Shahnaz akter" 3"Rozina khatun" 4"Tahania Farhin" 5"Hosne ara"
label val a0 a0

*Household size

#delimit;
recode a1 (min/3=1 "Small") 
(4/6=2 "Medium") (7/max=3 "Large"), 
gen(HH_size);
#delimit cr

*Roof material

destring a3, replace
label def a3 1"Pucca" 2"Tin" 3"Tin & bamboo" 4"Tin & others" 5"Bamboo & others" 6"Other"
label val a3 a3

recode a3 (1=1 "Yes") (2/6=0 "No"), gen(w1)

*Wall material

destring a4, replace
label def a4 1"Pucca" 2"Tin" 3"Tin & bamboo" 4"Tin & others" 5"Bamboo & others" 6"Wood" 7"Other"
label val a4 a4

recode a4 (1=1 "Yes") (2/7=0 "No"), gen(w2)

*Floor material

destring a5, replace
label def a5 1"Pucca" 2"Soil" 3"Wood" 4"Other"
label val a5 a5

recode a5 (1=1 "Yes") (2/4=0 "No"), gen(w3)

*Source of drinking water

destring a6, replace
label def a6 1"Supply water" 2"Deep tube-well" 3"Pond" 4"River" 5"Canal" 6"Other"
label val a6 a6

*List of household asset

order a77 a713, before (a7)

rename a71 w4

rename a72 w5

rename a73 w6

rename a74 w7

rename a75 w8

rename a76 w9

*rename a77 w4

rename a78 w10

rename a79 w11

rename a710 w12

rename a711 w13

rename a712 w14

*rename a713 w4

rename a714 w15

rename a715 w16

rename a716 w17

rename a717 w18

rename a718 w19

rename a719 w20

rename a720 w21

rename a721 w22

rename a722 w23

rename a723 w24

rename a724 w25

rename a725 w26

rename a726 w27

rename a727 w28

rename a728 w29

rename a729 w30

*Asset quintile
order w1 w2 w3, before (w4)
factor w1 - w30, pcf
predict asset_score
xtile asset_quintile=asset_score, nq(5)

#delimit;
label def asset_quintile 1"1st quintile"
2"2nd quintile" 3"3rd quintile" 4"4th quintile" 
5"5th quintile";
#delimit cr
label val asset_quintile asset_quintile

***********************************************

*Section B: Respondents Information

*Respondents birth year

replace b110_a=1995 if b110_a==9998 & b111==29
replace b110_a=1981 if b110_a==9998 & b111==43
replace b110_a=1993 if b110_a==9998 & b111==31

*Respondents birth month

destring b110_b, replace
label def b110_b 1"January" 2"February" 3"March" 4"April" 5"May" 6"June" 7"July" 8"August" 9"September" 10"October" 11"November" 12"December" 98"Don't know"
label val b110_b b110_b

*Respondents age at last birthday

gen age = 2024 - b110_a

#delimit;
recode age (min/19=1 "Less than 20") (20/24=2 "20-24 years")
(25/29=3 "25-29 years") (30/34=4 "30-34 years")
(35/39=5 "35-39 years") (40/44=6 "40-44 years")
(45/49=7 "45-49 years"), gen(resp_age);
#delimit cr

*Respondents marital status

destring b111_a, replace
label def b111_a 1"Currently married" 2"Separated" 3"Deserted" 4"Divorced" 5"Widow" 6"Never married"
label val b111_a b111_a

*Respondents health condition during interview

destring b112, replace
label def b112 1"Very good" 2"Good" 3"Medium" 4"Bad" 5"Very bad"
label val b112 b112

*Respondents self reported health score

recode b112_a (min/49=1 "Below 50") (50/max=2 "50 or above"), gen(self_hscore)

*Respondents attended school/madrasa

destring b113, replace
label def b113 1"Yes" 2"No"
label val b113 b113

*Respondents educational institution

destring b113_a, replace
label def b113_a 1"School" 2"Madrasa"
label val b113_a b113_a

*Type of madrasa education

destring b113_b, replace
label def b113_b 1"Alia/Dakhil" 2"Kowmi"
label val b113_b b113_b

*Respondents last completed degree

destring b114, replace
label def b114 1"Primary" 2"Secondary" 3"Higher secondary"
label val b114 b114

*Respondents reading capability (upto secondary)

destring b117, replace
label def b117 1"Can't read at all" 2"Can read partial" 3"Can read fully" 4"Language barrier" 5"Blind"
label val b117 b117

*Frequency of respondents newspaper read

destring b119, replace
label def b119 1"Minimum once a week" 2"Less than once a week" 3"Don't read at all"
label val b119 b119

*Frequency of respondents radio hear

destring b120, replace
label def b120 1"Minimum once a week" 2"Less than once a week" 3"Don't hear at all"
label val b120 b120

*Frequency of respondents television watch

destring b121, replace
label def b121 1"Minimum once a week" 2"Less than once a week" 3"Don't watch at all"
label val b121 b121

*Respondents mobile phone ownership

destring b122, replace
label def b122 1"Yes" 2"No"
label val b122 b122

*Type of respondents mobile phone

destring b123, replace
label def b123 1"Yes, smart phone" 2"Yes, basic phone" 3"Both" 4"Other"
label val b123 b123

*Religion of the respondent

destring b130, replace
label def b130 1"Muslim" 2"Hindu" 3"Buddhist" 4"Christian" 5"Other"
label val b130 b130

***********************************************

*Section C: Reproduction Information

*Respondents child bearing status

destring c201, replace
label def c201 1"Yes" 2"No"
label val c201 c201

*Having son/daughter living with the respondent (yes/no)

destring c202, replace
label def c202 1"Yes" 2"No"
label val c202 c202

*Having son/daughter not living with the respondent (yes/no)

destring c204, replace
label def c204 1"Yes" 2"No"
label val c204 c204

*Have given live birth but died later (yes/no)

destring c206, replace
label def c206 1"Yes" 2"No"
label val c206 c206

*Total live birth confirmation (yes/no)

destring c209, replace
label def c209 1"Yes" 2"No"
label val c209 c209

*Experienced dead birth, miscarriage, abortion (yes/no)

destring c210, replace
label def c210 1"Yes" 2"No"
label val c210 c210

*Total number of pregnancy confirmation (yes/no)

destring c212_1, replace
label def c212_1 1"Yes" 2"No"
label val c212_1 c212_1

***********************************************

*Pregnancy after the last pregnancy

destring c222_A, replace
label def c222_A 1"Yes" 2"No"
label val c222_A c222_A

*Checking the serial of the pregnancy

destring c222_B, replace
label def c222_B 1"Yes" 2"No"
label val c222_B c222_B

***********************************************

*Current pregnancy status of the respondent

destring c232, replace
label def c232 1"Yes" 2"No"
label val c232 c232

*Current pregnancy intentional or unintentional

destring c234, replace
label def c234 1"Yes" 2"No"
label val c234 c234

*Wantedness of the pregnancy

destring c235_a, replace
label def c235_a 1"Wanted child later" 2"Wanted no more child"
label val c235_a c235_a

destring c235_b, replace
label def c235_b 1"Wanted child later" 2"Wanted no child"
label val c235_b c235_b

*Consumed iron, folic acid in last 3 month

destring c235B, replace
label def c235B 1"Yes" 2"No" 3"Don't know"
label val c235B c235B

replace c235B=. if _index==33

*Last menstruation calculation

destring c236, replace
label def c236 1"Days ago" 2"Week ago" 3"Month ago" 4"Year ago" 5"Can tell date" 994"Menopause/Hysterectomy" 995"Before last pregnancy" 996"Never menstruated"
label val c236 c236

*Last menstruation within 1 year

destring c237, replace
label def c237 1"Yes, in one year" 2"No, one year or more"
label val c237 c237

*Menstruation material used

destring c238, replace
label def c238 1"Reusable sanitary pad" 2"Disposable sanitary pad" 3"Tampons" 4"Menstrual cup" 5"Cloth" 6"Toilet paper" 7"Cotton pad" 8"Underwear only" 9"Nothing" 10"Other"
label val c238 c238

*Able to maintain confidentiality during menstruation

destring c239, replace
label def c239 1"Yes" 2"No" 3"Was away from home"
label val c239 c239

*Remember age during first menstruation

destring c240, replace
label def c240 1"Yes" 2"No"
label val c240 c240

*Age during first menstruation

#delimit;
recode c240_a (min/12=1 "10-12 years") 
(13/14=2 "13-14 years") (15/max=3 "15 or more years"),
gen (age_mens);
#delimit cr

*Knowledge about pregnancy possibility time (yes/no)

destring c241, replace
label def c241 1"Yes" 2"No" 3"Don't know"
label val c241 c241

*Timing of highest pregnancy possibility

destring c242, replace
label def c242 1"Just before menstruation" 2"During mentruation" 3"Just after menstruation" 4"Halfway between 2 periods" 5"Don't know" 6"Other"
label val c242 c242

*Pregnancy in between child birth & menstruation resume

destring c243, replace
label def c243 1"Yes" 2"No" 3"Don't know"
label val c243 c243

*Anyone else nearby during the interview of "section C"

destring c244, replace
label def c244 1"Full time" 2"Partial" 3"No one"
label val c244 c244

***********************************************

*Section D: Interviewers observation

*Interview taken inside or outside the house

destring d1, replace
label def d1 1"In house" 2"Outside"
label val d1 d1

*Interview interruption

destring d2, replace
label def d2 1"Yes" 2"No"
label val d2 d2

*Interview interruption time

destring d3, replace
label def d3 1"Less than 5 minutes" 2"5-10 minutes" 3"More than 10 minutes"
label val d3 d3

*Enumerators familiarity with the respondent

destring d4, replace
label def d4 1"Not at all" 2"Very little" 3"Well" 4"Very well"
label val d4 d4

*Enumerators familiarity with other members

destring d5, replace
label def d5 1"Yes" 2"No"
label val d5 d5

*How much familiar was the other members

destring d6, replace
label def d6 1"Very little" 2"Well" 3"Very well"
label val d6 d6

*Respondents co-operation during the interview

destring d7, replace
label def d7 1"Very good" 2"Good" 3"Normal" 4"Bad" 5"Very bad"
label val d7 d7

*Anyone else nearby during the whole interview

destring d8, replace
label def d8 1"Yes" 2"No"
label val d8 d8

***********************************************

*Saving parent file

save final_parent.dta, replace

***********************************************
***********************************************

import excel "aXmwdKgwzBQDSJH6mm7jjM_2025_01_27_14_26_50.xlsx", sheet("Reproduction_section_Pregnancy_") firstrow clear

***********************************************

*Rename Variables

rename Reproduction_sectionPregnancy_h c200
rename B c215_a
rename C c215
rename D c216
rename E c216_a
rename F cal_216
rename G c217
rename H c218
rename I c219
rename J c220
rename K c220_a
rename L c221_aa
rename M c221
rename N c221_a
rename O c221_aaa
rename P c222
rename Q c222_a
rename R c223
rename S cal_223
rename T c224
rename U c225
rename V c226
rename W c228
rename X c228_aa
rename Y c228_bb
rename Z c228_ccc
rename AA c228_cc
rename AB c228_d
rename AC c228_A
rename AD c228_B
rename AE c228_C

***********************************************

*Labeling the variables

label variable c200 "Section C2 start time"
label variable c215_a "Pregnancy serial number"
label variable c215 "Pregnancy type"
label variable c216 "Pregnancy outcome type"
label variable c216_a "Pregnancy outcome type 2"
label variable cal_216 "Pregnancy outcome type (auto)"
label variable c217 "Movement after birth"
label variable c218 "Name of the children"
label variable c219 "Gender of the children"
label variable c220 "Childrens birth date"
label variable c220_a "Pregnancy ending date"
label variable c221_aa "Pregnancy ending time (months/weeks)"
label variable c221 "Pregnancy ending time (months)"
label variable c221_a "Pregnancy ending time (weeks)"
label variable c221_aaa "Pregnancy ending time (text)"
label variable c222 "Other pregnancy before"
label variable c222_a "Other pregnancy in between"
label variable c223 "Pregnancy outcome type (generated)"
label variable cal_223 "Current condition of the pregnancy"
label variable c224 "Children alive/dead"
label variable c225 "Age at last birthday"
label variable c226 "Living with the respondent"
label variable c228 "Age at death (day/month/year)"
label variable c228_aa "Age at death (day)"
label variable c228_bb "Age at death (month)"
label variable c228_ccc "Age at death (year)"
label variable c228_cc "Age at death (text)"
label variable c228_d "Completed 1st birthday"
label variable c228_A "Death within 4 years or more"
label variable c228_B "Date of death"
label variable c228_C "Death before/after 2017"

***********************************************

*Section C.a: Pregnancy history

*Type of the pregnancy (single/ twin/ triple/ more)

destring c215, replace
label def c215 1"Single" 2"Twin" 3"Triple" 4"More than 3"
label val c215 c215

*Type of the pregnancy outcome

destring c216, replace
label def c216 1"Born alive" 2"Born dead" 3"Miscarriage" 4"Abortion"
label val c216 c216

destring c216_a, replace
label def c216_a 1"Born alive" 2"Born dead"
label val c216_a c216_a

*Move/ cry of the born dead child

destring c217, replace
label def c217 1"Yes" 2"No"
label val c217 c217

*Calculation of c216 (Type of the pregnancy outcome)

replace cal_216="1" if cal_216=="জীবিত জন্ম"
replace cal_216="2" if cal_216=="মৃত জন্ম"
replace cal_216="3" if cal_216=="গর্ভনষ্ট (এমনিতে নষ্ট হয়ে গেছে)"
replace cal_216="4" if cal_216=="গর্ভপাত (নিজে নষ্ট করেছে)"

destring cal_216, replace
label def cal_216 1"Born alive" 2"Born dead" 3"Miscarriage" 4"Abortion"
label val cal_216 cal_216

replace cal_216=c216_a if cal_216==.

*Sex of the child

destring c219, replace
label def c219 1"Boy" 2"Girl"
label val c219 c219

*Pregnancy duration (month/ week)

destring c221_aa, replace
label def c221_aa 1"Month" 2"Week"
label val c221_aa c221_aa

*Other pregnancy before the first pregnancy

destring c222, replace
label def c222 1"Yes" 2"No"
label val c222 c222

*Other pregnancy in between two pregnancies

destring c222_a, replace
label def c222_a 1"Yes" 2"No"
label val c222_a c222_a

*Outcome of the pregnancy (calculated)

destring c223, replace
label def c223 1"Born alive" 2"Born dead" 3"Miscarriage" 4"Abortion"
label val c223 c223

*Current life status of the birth

destring c224, replace
label def c224 1"Yes" 2"No"
label val c224 c224

*Age of the alive son/ daughter

#delimit;
recode c225 (0=1 "Less than one year") (1/4=2 "1-4 years")
(5/9=3 "5-9 years") (10/15=4 "10-15 years")
(16/19=5 "16-19 years") (20/max=6 "20 years or more"),
gen(age_alive);
#delimit cr

*Alive children living together with the respondent

destring c226, replace
label def c226 1"Yes" 2"No"
label val c226 c226

*Childrens age at death who born alive (day/ month/ year)
 
destring c228, replace
label def c228 1"In days" 2"In months" 3"In years"
label val c228 c228

*Son/ daughters completed first birthday (born alive then died)

destring c228_d, replace
label def c228_d 1"Yes" 2"No"
label val c228_d c228_d

*Son/ daughters age at death who born alive (category)

destring c228_A, replace
label def c228_A 1"0-4 years" 2"5 years or more"
label val c228_A c228_A

*Son/ daughters period of death who born alive

destring c228_C, replace
label def c228_C 1"2018 or later" 2"2017 or before"
label val c228_C c228_C

***********************************************

*Saving reproductive file

save final_rh.dta, replace

***********************************************

*Merging two sheets of the excel file

use final_rh.dta, clear 

rename _index _index1

rename _parent_index _index

merge m:1 _index using final_parent.dta, force

***********************************************

*Saving final dataset

save final_all.dta, replace

***********************************************
***********************************************

use final_parent.dta

tab x1
tab1 b110_a b110_b b111 resp_age b111_a b112 b112_a self_hscore b113 b113_a b113_b b114 b115 b117 b117_a b119 b120 b121 b122 b123 b123_a b130 b130_a c100 c201 c202 c203_a c203_aa c203_b c203_bb c204 c205_a c205_aa c205_b c205_bb c206 c207_a c207_aa c207_b c207_bb c208 c209 c210 c211_a c212 c212_1 c230_A c222_A c222_B c300 c232 c233 c233_a c234 c235_a c235_b c235B c236 c236_a c236_b c236_c c236_d c236_e c237 c238 c238_a c239 c239A c240 age_mens c241 c242 c242_a c243 c244 d1 d2 d3 d4 d5 d6 d7 d8 if x1==1, miss

*Tab operating start

tab start

*Tab operating end

tab end

*Interview start time

tab aa

*First serial number

tab serial1

*Second serial number

tab serial2

*Third serial number (Generated by enumerator)

tab s3

*Third serial number (Given by the office)

tab serial3

*Respondent's HH visiting number

tab x0

*Date of the 1st visit to the HH

tab x0_1

*Date of the 2nd visit to the HH

tab x0_2

*Date of the 3rd visit to the HH

tab x0_3

*Interview status

tab x1

***********************************************

*Section A: Respondents Information

*Section A start time

tab a00

*Name of the respondent

tab a0_1

*Interviewer wise data number

tab a0

*Household size

tab HH_size

*Roof material

tab w1

*Wall material

tab w2

*Floor material

tab w3

*Source of drinking water

tab a6_a

*Asset quintile

tab asset_quintile

*Section A end time

tab a000

***********************************************

*Section B: Respondents Information

*Section B start time

tab b00

*Respondents birth year

tab b110_a

*Respondents birth month

tab b110_b

*Respondents age at last birthday

tab b111

tab resp_age

*Respondents marital status

tab b111_a

*Respondents health condition during interview

tab b112

*Respondents self reported health score

tab self_hscore

*Respondents attended school/madrasa

tab b113

*Respondents educational institution

tab b113_a

*Type of madrasa education

tab b113_b

*Respondents last completed degree

tab b114

*Respondents last completed class

tab b115

*Respondents reading capability (upto secondary)

tab b117

*Frequency of respondents newspaper read

tab b119

*Frequency of respondents radio hear

tab b120

*Frequency of respondents television watch

tab b121

*Respondents mobile phone ownership

tab b122

*Type of respondents mobile phone

tab b123

*Religion of the respondent

tab b130

*Section B end time

tab b000

***********************************************

*Section C: Reproduction Information

*Section C100 start time

tab c100

*Respondents child bearing status

tab c201

*Having son/daughter living with the respondent (yes/no)

tab c202

*Number of son living with the respondent

tab c203_a

*Number of daughter living with the respondent

tab c203_b

*Having son/daughter not living with the respondent (yes/no)

tab c204

*Number of son not living with the respondent

tab c205_a

*Number of daughter not living with the respondent

tab c205_b

*Have given live birth but died later (yes/no)

tab c206

*Number of son born alive but died llater

tab c207_a

*Number of daughter born alive but died llater

tab c207_b

*Total live birth confirmation (yes/no)

tab c209

*Experienced dead birth, miscarriage, abortion (yes/no)

tab c210

*Number of dead birth, miscarriage, abortion

tab c211

*Total number of pregnancy of the respondent

tab c212

*Total number of pregnancy confirmation (yes/no)

tab c212_1

*Section C1000 end time

tab c1000

***********************************************

*Total number of death of son/ daughter in between 2018 & 2023

tab c230_A

*Pregnancy after the last pregnancy

tab c222_A

*Checking the serial of the pregnancy

tab c222_B

*Section C2000 end time

tab c2000

***********************************************

*Section C300 start time

tab c300

*Current pregnancy status of the respondent

tab c232

*Current pregnancy period (in week)

tab c233

*Current pregnancy period (in month)

tab c233_a

*Current pregnancy intentional or unintentional

tab c234

*Wantedness of the pregnancy

tab c235_a

tab c235_b

*Consumed iron, folic acid in last 3 month

tab c235B

*Last menstruation calculation

tab c236

*Last menstruation calculation (day)

tab c236_a

*Last menstruation calculation (week)

tab c236_b

*Last menstruation calculation (month)

tab c236_c

*Last menstruation calculation (year)

tab c236_d

*Last menstruation calculation (date)

tab c236_e

*Last menstruation within 1 year

tab c237

*Menstruation material used

tab c238

*Able to maintain confidentiality during menstruation

tab c239

*Work restriction during menstruation

tab c239A

ssc install mrtab //multiple response

mrtab c239A1 c239A2 c239A3 c239A4 c239A5 c239A6 c239A7 c239A8 c239A9 c239A10 c239A11 c239A12

*Remember age during first menstruation

tab c240

*Age during first menstruation

tab age_mens

*Knowledge about pregnancy possibility time (yes/no)

tab c241

*Timing of highest pregnancy possibility

tab c242

*Pregnancy in between child birth & menstruation resume

tab c243

*Anyone else nearby during the interview of "section C"

tab c244

*Section C3000 end time

tab c3000

***********************************************

*Section D: Interviewers observation

*Section d00 start time

tab d00

*Interview taken inside or outside the house

tab d1

*Interview interruption

tab d2

*Interview interruption time

tab d3

*Enumerators familiarity with the respondent

tab d4

*Enumerators familiarity with other members

tab d5

*How much familiar was the other members

tab d6

*Respondents co-operation during the interview

tab d7

*Anyone else nearby during the whole interview

tab d8

*Any other comments related to the interview

tab d9

*Section d000 end time

tab d000

***********************************************

*Interview end time

tab dd

********************Recorded variables

tab1 HH_size asset_score asset_quintile age resp_age self_hscore age_mens


***********************************************
***********************************************

*Section C.a: Pregnancy history
use final_rh.dta, clear

tab1 c215_a c215 c216 c216_a cal_216 c217 c218 c219 c220 c220_a c221_aa c221 c221_a c221_aaa c222 c222_a c223 cal_223 c224 c225 age_alive c226 c228 c228_aa c228_bb c228_ccc c228_cc c228_d c228_A c228_B c228_C, miss

*Section C200 start time

tab c200

*Serial number of the pregnancy

tab c215_a

*Type of the pregnancy (single/ twin/ triple/ more)

tab c215

*Type of the pregnancy outcome

tab c216

tab c216_a

*Move/ cry of the born dead child

tab c217

*Calculation of c216 (Type of the pregnancy outcome)

tab cal_216

*Name of the child

tab c218

*Sex of the child

tab c219

*Bith date of the child

tab c220

*Pregnancy ending time (Exact date)

tab c220_a

*Pregnancy duration (month/ week)

tab c221_aa

*Pregnancy duration in month

tab c221

*Pregnancy duration in week

tab c221_a

*Pregnancy duration in text format

tab c221_aaa

*Other pregnancy before the first pregnancy

tab c222

*Other pregnancy in between two pregnancies

tab c222_a

*Outcome of the pregnancy (calculated)

tab c223

*Current life status of the birth

tab c224

*Age of the alive son/ daughter

tab age_alive

*Alive children living together with the respondent

tab c226

*Childrens age at death who born alive (day/ month/ year)

tab c228

*Son/ daughters age at death who born alive (day)

tab c228_aa

*Son/ daughters age at death who born alive (month)

tab c228_bb

*Son/ daughters age at death who born alive (year)

tab c228_ccc

*Son/ daughters age at death who born alive (text)

tab c228_cc

*Son/ daughters completed first birthday (born alive then died)

tab c228_d

*Son/ daughters age at death who born alive (category)

tab c228_A

*Son/ daughters date of death who born alive

tab c228_B

*Son/ daughters period of death who born alive

tab c228_C

***********************************************
***********************************************










