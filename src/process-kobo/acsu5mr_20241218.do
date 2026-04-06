set more off
use Parent_file_18_12_2024.dta, clear 
***********************************************

*RENAME VARIABLES

rename begin_group_qiWDTpn53serial1 serial1
rename begin_group_qiWDTpn53serial2 serial2
rename begin_group_qiWDTpn53s3 s3
rename begin_group_qiWDTpn53serial3 serial3

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
rename DA c230_A
rename DB c222_A
rename DC c222_B
rename DD c2000

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

***********************************************

*Section A: Respondents Information

*Section A start time
tab a00

*Name of the respondent
tab a0_1

*Interviewers info

destring a0, replace
label def a0 1"Mofazzal hossain" 2"Shahnaz akter" 3"Rozia khatun" 4"Tahania Farhin" 5"Hosne ara"
label val a0 a0

tab a0
tab a0, nolabel

*Household member
tab a1

*Roof material

destring a3, replace
label def a3 1"Pucca" 2"Tin" 3"Tin & bamboo" 4"Tin & others" 5"Bamboo & others" 6"Other"
label val a3 a3

tab a3
tab a3, nolabel

*Wall material

destring a4, replace
label def a4 1"Pucca" 2"Tin" 3"Tin & bamboo" 4"Tin & others" 5"Bamboo & others" 6"Wood" 7"Other"
label val a4 a4

tab a4
tab a4, nolabel

*Floor material

destring a5, replace
label def a5 1"Pucca" 2"Soil" 3"Wood" 4"Other"
label val a5 a5

tab a5
tab a5, nolabel

*Source of drinking water

destring a6, replace
label def a6 1"Supply water" 2"Deep tube-well" 3"Pond" 4"River" 5"Canal" 6"Other"
label val a6 a6

tab a6
tab a6, nolabel

*List of household asset
tab a7

*Section A end time
tab a000

***********************************************

*Section B: Respondents Information

*Section B start time
tab b00

*Respondents birth year
tab b110_a

*Respondents birth month

destring b110_b, replace
label def b110_b 1"January" 2"February" 3"March" 4"April" 5"May" 6"June" 7"July" 8"August" 9"September" 10"October" 11"November" 12"December" 98"Don't know"
label val b110_b b110_b

tab b110_b
tab b110_b, nolabel

*Respondents age at last birthday
tab b111

*Respondents marital status

destring b111_a, replace
label def b111_a 1"Currently married" 2"Separated" 3"Deserted" 4"Divorced" 5"Widow" 6"Never married"
label val b111_a b111_a

tab b111_a
tab b111_a, nolabel

*Respondents health condition during interview

destring b112, replace
label def b112 1"Very good" 2"Good" 3"Medium" 4"Bad" 5"Very bad"
label val b112 b112

tab b112
tab b112, nolabel

*Respondents self reported health score
tab b112_a

*Respondents education (school/madrasa)

destring b113, replace
label def b113 1"Yes" 2"No"
label val b113 b113

tab b113
tab b113, nolabel

*Respondents educational institution

destring b113_a, replace
label def b113_a 1"School" 2"Madrasa"
label val b113_a b113_a

tab b113_a
tab b113_a, nolabel

*Type of madrasa education

destring b113_b, replace
label def b113_b 1"Alia/Dakhil" 2"Kowmi"
label val b113_b b113_b

tab b113_b
tab b113_b, nolabel

*Respondents last completed degree

destring b114, replace
label def b114 1"Primary" 2"Secondary" 3"Higher secondary"
label val b114 b114

tab b114
tab b114, nolabel

*Respondents last completed class
tab b115

*Respondents reading ability (upto secondary)

destring b117, replace
label def b117 1"Can't read at all" 2"Can read partial" 3"Can read fully" 4"Language barrier" 5"Blind"
label val b117 b117

tab b117
tab b117, nolabel

*Frequency of respondents newspaper read

destring b119, replace
label def b119 1"Minimum once a week" 2"Less than once a week" 3"Don't read at all"
label val b119 b119

tab b119
tab b119, nolabel

*Frequency of respondents radio hear

destring b120, replace
label def b120 1"Minimum once a week" 2"Less than once a week" 3"Don't hear at all"
label val b120 b120

tab b120
tab b120, nolabel

*Frequency of respondents television watch

destring b121, replace
label def b121 1"Minimum once a week" 2"Less than once a week" 3"Don't watch at all"
label val b121 b121

tab b121
tab b121, nolabel

*Respondents mobile phone ownership

destring b122, replace
label def b122 1"Yes" 2"No"
label val b122 b122

tab b122
tab b122, nolabel

*Type of respondents mobile phone

destring b123, replace
label def b123 1"Yes, smart phone" 2"Yes, basic phone" 3"Both" 4"Other"
label val b123 b123

tab b123
tab b123, nolabel

*Religion of the respondent

destring b130, replace
label def b130 1"Muslim" 2"Hindu" 3"Buddhist" 4"Christian" 5"Other"
label val b130 b130

tab b130
tab b130, nolabel

*Section B end time
tab b000

***********************************************

*Section C: Reproduction Information

*Section C100 start time
tab c100

*Respondents child bearing status

destring c201, replace
label def c201 1"Yes" 2"No"
label val c201 c201

tab c201
tab c201, nolabel

*Son/daughter living with the respondent (yes/no)

destring c202, replace
label def c202 1"Yes" 2"No"
label val c202 c202

tab c202
tab c202, nolabel

*Number of son living with the respondent
tab c203_a

*Number of daughter living with the respondent
tab c203_b

*Son/daughter not living with the respondent (yes/no)

destring c204, replace
label def c204 1"Yes" 2"No"
label val c204 c204

tab c204
tab c204, nolabel

*Number of son not living with the respondent
tab c205_a

*Number of daughter not living with the respondent
tab c205_b

*Given live birth but died later (yes/no)

destring c206, replace
label def c206 1"Yes" 2"No"
label val c206 c206

tab c206
tab c206, nolabel

*Number of son born alive but died llater
tab c207_a

*Number of daughter born alive but died llater
tab c207_b

*Total live birth confirmation (yes/no)

destring c209, replace
label def c209 1"Yes" 2"No"
label val c209 c209

tab c209
tab c209, nolabel

*Dead birth, miscarriage, abortion (yes/no)

destring c210, replace
label def c210 1"Yes" 2"No"
label val c210 c210

tab c210
tab c210, nolabel

*Number of dead birth, miscarriage, abortion
tab c211

*Total number of pregnancy of the respondent
tab c212

*Total number of pregnancy confirmation (yes/no)

destring c212_1, replace
label def c212_1 1"Yes" 2"No"
label val c212_1 c212_1

tab c212_1
tab c212_1, nolabel

*Section C1000 end time
tab c1000

***********************************************

*Total number of death of son/ daughter in between 2018 & 2023
tab c230_A

*Pregnancy after the last pregnancy

destring c222_A, replace
label def c222_A 1"Yes" 2"No"
label val c222_A c222_A

tab c222_A
tab c222_A, nolabel

*Checking the serial of the pregnancy

destring c222_B, replace
label def c222_B 1"Yes" 2"No"
label val c222_B c222_B

tab c222_B
tab c222_B, nolabel

*Section C2000 end time
tab c2000

***********************************************

*Section C300 start time
tab c300

*Current pregnancy status check

destring c232, replace
label def c232 1"Yes" 2"No"
label val c232 c232

tab c232
tab c232, nolabel

*Current pregnancy period (in week)
tab c233

*Current pregnancy period (in month)
tab c233_a

*Current pregnancy intended or unintended

destring c234, replace
label def c234 1"Yes" 2"No"
label val c234 c234

tab c234
tab c234, nolabel

*Wanted pregnancy later or never

destring c235_a, replace
label def c235_a 1"Wanted child later" 2"Wanted no more child"
label val c235_a c235_a

tab c235_a
tab c235_a, nolabel

destring c235_b, replace
label def c235_b 1"Wanted child later" 2"Wanted no child"
label val c235_b c235_b

tab c235_b
tab c235_b, nolabel

*Consumed iron, folic acid in last 3 month

destring c235B, replace
label def c235B 1"Yes" 2"No" 3"Don't know"
label val c235B c235B

tab c235B
tab c235B, nolabel

*Last menstruation calculation

destring c236, replace
label def c236 1"Days ago" 2"Week ago" 3"Month ago" 4"Year ago" 5"Can tell date" 994"Menopause/Hysterectomy" 995"Before last pregnancy" 996"Never menstruated"
label val c236 c236

tab c236
tab c236, nolabel

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

destring c237, replace
label def c237 1"Yes, in one year" 2"No, one year or more"
label val c237 c237

tab c237
tab c237, nolabel

*Menstruation material used

destring c238, replace
label def c238 1"Reusable sanitary pad" 2"Disposable sanitary pad" 3"Tampons" 4"Menstrual cup" 5"Cloth" 6"Toilet paper" 7"Cotton pad" 8"Underwear only" 9"Nothing" 10"Other"
label val c238 c238

tab c238
tab c238, nolabel

*Able to maintain confidentiality during menstruation

destring c239, replace
label def c239 1"Yes" 2"No" 3"Was away from home"
label val c239 c239

tab c239
tab c239, nolabel

*Work restriction during menstruation
tab c239A

mrtab c239A1 c239A2 c239A3 c239A4 c239A5 c239A6 c239A7 c239A8 c239A9 c239A10 c239A11 c239A12

*Remember age during first menstruation

destring c240, replace
label def c240 1"Yes" 2"No"
label val c240 c240

tab c240
tab c240, nolabel

*Age during first menstruation
tab c240_a

*Highest possibility of becoming pregnant (yes/no)

destring c241, replace
label def c241 1"Yes" 2"No" 3"Don't know"
label val c241 c241

tab c241
tab c241, nolabel

*Highest possibility of becoming pregnant (when)

destring c242, replace
label def c242 1"Just before menstruation" 2"During mentruation" 3"Just after menstruation" 4"Halfway between 2 periods" 5"Don't know" 6"Other"
label val c242 c242

tab c242
tab c242, nolabel

*Pregnancy in between child birth & menstruation resume

destring c243, replace
label def c243 1"Yes" 2"No" 3"Don't know"
label val c243 c243

tab c243
tab c243, nolabel

*Anyone else nearby during the interview of "section C"

destring c244, replace
label def c244 1"Full time" 2"Partial" 3"No one"
label val c244 c244

tab c244
tab c244, nolabel

*Section C3000 end time
tab c3000

***********************************************

*Section D: Interviewers observation

*Section d00 start time
tab d00

*Interview taken inside or outside the house

destring d1, replace
label def d1 1"In house" 2"Outside"
label val d1 d1

tab d1
tab d1, nolabel

*Interview interruption

destring d2, replace
label def d2 1"Yes" 2"No"
label val d2 d2

tab d2
tab d2, nolabel

*Interview interruption time

destring d3, replace
label def d3 1"Less than 5 minutes" 2"5-10 minutes" 3"More than 10 minutes"
label val d3 d3

tab d3
tab d3, nolabel

*Enumerators familiarity with the respondent

destring d4, replace
label def d4 1"Not at all" 2"Very little" 3"Well" 4"Very well"
label val d4 d4

tab d4
tab d4, nolabel

*Enumerators familiarity with other members

destring d5, replace
label def d5 1"Yes" 2"No"
label val d5 d5

tab d5
tab d5, nolabel

*How much familiar was the other members

destring d6, replace
label def d6 1"Very little" 2"Well" 3"Very well"
label val d6 d6

tab d6
tab d6, nolabel

*Respondents co-operation during the interview

destring d7, replace
label def d7 1"Very good" 2"Good" 3"Normal" 4"Bad" 5"Very bad"
label val d7 d7

tab d7
tab d7, nolabel

*Anyone else nearby during the interview

destring d8, replace
label def d8 1"Yes" 2"No"
label val d8 d8

tab d8
tab d8, nolabel

*Any other comments related to the interview
tab d9

*Section d000 end time
tab d000

***********************************************

*Interview end time
tab dd

***********************************************
***********************************************
set more off
use Sheet_1_18_12_2024.dta, clear 

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

*Section C.a: Pregnancy history

*Section C200 start time
tab c200

*Serial number of the pregnancy
tab c215_a

*Type of the pregnancy (single/ twin/ triple/ more)

destring c215, replace
label def c215 1"Single" 2"Twin" 3"Triple" 4"More than 3"
label val c215 c215

tab c215
tab c215, nolabel

*Type of the pregnancy outcome

destring c216, replace
label def c216 1"Born alive" 2"Born dead" 3"Miscarriage" 4"Abortion"
label val c216 c216

tab c216
tab c216, nolabel

destring c216_a, replace
label def c216_a 1"Born alive" 2"Born dead"
label val c216_a c216_a

tab c216_a
tab c216_a, nolabel

*Move/ cry of the born dead child

destring c217, replace
label def c217 1"Yes" 2"No"
label val c217 c217

tab c217
tab c217, nolabel

*Name of the child
tab c218

*Sex of the child

destring c219, replace
label def c219 1"Boy" 2"Girl"
label val c219 c219

tab c219
tab c219, nolabel

*Bith date of the child
tab c220

*Pregnancy ending time
tab c220_a

*Pregnancy duration (month/ week)

destring c221_aa, replace
label def c221_aa 1"Month" 2"Week"
label val c221_aa c221_aa

tab c221_aa
tab c221_aa, nolabel

*Pregnancy duration in month
tab c221

list _parent_index _index if c221==36

*Pregnancy duration in week
tab c221_a

list _parent_index _index if c221_a==6 | c221_a==7 | c221_a==8 | c221_a==9 | c221_a==10

*Pregnancy duration in text format
tab c221_aaa

*Other pregnancy before the first pregnancy

destring c222, replace
label def c222 1"Yes" 2"No"
label val c222 c222

tab c222
tab c222, nolabel

*Other pregnancy in between two pregnancies

destring c222_a, replace
label def c222_a 1"Yes" 2"No"
label val c222_a c222_a

tab c222_a
tab c222_a, nolabel

*Outcome of the pregnancy (calculated)

destring c223, replace
label def c223 1"Born alive" 2"Born dead" 3"Miscarriage" 4"Abortion"
label val c223 c223

tab c223
tab c223, nolabel

*Current life status of the birth

destring c224, replace
label def c224 1"Yes" 2"No"
label val c224 c224

tab c224
tab c224, nolabel

*Age of the alive son/ daughter
tab c225

*Son/ daughter living together with the respondent

destring c226, replace
label def c226 1"Yes" 2"No"
label val c226 c226

tab c226
tab c226, nolabel

*Son/ daughters age at death who born alive (day/ month/ year)
 
destring c228, replace
label def c228 1"In days" 2"In months" 3"In years"
label val c228 c228

tab c228
tab c228, nolabel

*Son/ daughters age at death who born alive (day)
tab c228_aa

*Son/ daughters age at death who born alive (month)
tab c228_bb

*Son/ daughters age at death who born alive (year)
tab c228_ccc

*Son/ daughters age at death who born alive (text)
tab c228_cc

*Son/ daughters completed first birthday (born alive then died)

destring c228_d, replace
label def c228_d 1"Yes" 2"No"
label val c228_d c228_d

tab c228_d
tab c228_d, nolabel

*Son/ daughters age at death who born alive (category)

destring c228_A, replace
label def c228_A 1"0-4 years" 2"5 years or more"
label val c228_A c228_A

tab c228_A
tab c228_A, nolabel

*Son/ daughters date of death who born alive
tab c228_B

*Son/ daughters period of death who born alive

destring c228_C, replace
label def c228_C 1"2018 or later" 2"2017 or before"
label val c228_C c228_C

tab c228_C
tab c228_C, nolabel

***********************************************




















