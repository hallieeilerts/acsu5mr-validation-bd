
*You have to change the directory (Line number-4) change data file name in two places (Line number-8; Line number-2119)

cd "C:\Users\HEilerts\Institute of International Programs Dropbox\Hallie Eilerts-Spinelli\ACSU5MR\acsu5mr-validation-bd\data"  //Change the directory as per the file location


***********************************************
import excel "aXmwdKgwzBQDSJH6mm7jjM_2025_03_24_04_09_34.xlsx", sheet("aXmwdKgwzBQDSJH6mm7jjM") firstrow clear //Change the excel file name 

***********************************************

*Dropping dummy data

*drop if serial1=="9999"

drop if _index>352 & _index<393

***********************************************

*RENAME VARIABLES

rename begin_group_9aCf6tU8fserial1 serial1
rename begin_group_9aCf6tU8fserial2 serial2
rename begin_group_9aCf6tU8fs3 s3
rename begin_group_9aCf6tU8fserial3 serial3

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
rename Reproduction_sectionc211 c211
rename Reproduction_sectionc211_a c211_a
rename Reproduction_sectionc212 c212
rename Reproduction_sectionc212_1 c212_1
rename Reproduction_sectionc1000 c1000

rename Reproduction_sectionPregnancy_h preg_num
rename DG c222_A
rename DH c222_B
rename DI c2000

rename Reproduction_sectionc244 c244

rename Reproduction_sectionc244_a c244_a
rename Reproduction_sectionc244_a1 c244_a1
rename Reproduction_sectionc244_a2 c244_a2
rename Reproduction_sectionc244_a3 c244_a3
rename Reproduction_sectionc244_a4 c244_a4
rename Reproduction_sectionc244_a5 c244_a5
rename Reproduction_sectionc244_a_1 c244_a_1
rename Reproduction_sectionc244_a_2 c244_a_2
rename Reproduction_sectionc244_a_3 c244_a_3
rename Reproduction_sectionc244_a_4 c244_a_4
rename Reproduction_sectionc244_a_5 c244_a_5

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

rename Interviewers_Observationd10 d10
rename Interviewers_Observationd10_a d10_a
rename Interviewers_Observationd10_a1 d10_a1
rename Interviewers_Observationd10_a2 d10_a2
rename Interviewers_Observationd10_a3 d10_a3
rename Interviewers_Observationd10_a4 d10_a4
rename Interviewers_Observationd10_a5 d10_a5
rename Interviewers_Observationd10_a6 d10_a6
rename Interviewers_Observationd10_a_1 d10_a_1
rename Interviewers_Observationd10_a_2 d10_a_2
rename Interviewers_Observationd11 d11
rename Interviewers_Observationd11_a d11_a
rename Interviewers_Observationd12 d12
rename Interviewers_Observationd12_a d12_a
rename Interviewers_Observationd12_a1 d12_a1
rename Interviewers_Observationd12_a2 d12_a2
rename Interviewers_Observationd12_a3 d12_a3
rename Interviewers_Observationd12_a4 d12_a4
rename Interviewers_Observationd12_a_1 d12_a_1
rename Interviewers_Observationd13 d13
rename Interviewers_Observationd13_a d13_a
rename Interviewers_Observationd14 d14

rename Interviewers_Observationd9 d9
rename Interviewers_Observationd000 d000

***********************************************

*Labeling the variables

label variable start "Time stamp of interview start"
label variable end "Time stamp of interview end"

label variable aa "Interview start time"

label variable serial1 "1st serial"
label variable serial2 "2nd serial"
label variable s3 "3rd serial (generated)"
label variable serial3 "3rd serial (provided)"

label variable x0 "Visit time"
label variable x0_1 "1st visit date"
label variable x0_2 "2nd visit date"
label variable x0_3 "3rd visit date"
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
label variable c203_a "Son living with respondent"
label variable c203_aa "Son living with respondent (Calculate)"
label variable c203_b "Daughter living with respondent"
label variable c203_bb "Daughter living with respondent (Calculate)"
label variable c204 "Alive children not living with respondent"
label variable c205_a "Son not living with respondent"
label variable c205_aa "Son not living with respondent (Calculate)"
label variable c205_b "Daughter not living with respondent"
label variable c205_bb "Daughter not living with respondent (Calculate)"
label variable c206 "Children born alive but died later"
label variable c207_a "Son born alive but died later"
label variable c207_aa "Son born alive but died later (Calculate)"
label variable c207_b "Daughter born alive but died later"
label variable c207_bb "Daughter born alive but died later (Calculate)"
label variable c208 "Total child born alive (Calculate)"
label variable c209 "Total child number accuracy"
label variable c210 "Born dead/miscarriage/abortion (yes/no)"
label variable c211 "Stillbirth/miscarriage/abortion number"
label variable c211_a "Stillbirth/miscarriage/abortion number (Calculate)"
label variable c212 "Total number of pregnancy (Calculate)"
label variable c212_1 "Total pregnancy accuracy"
label variable c1000 "Section C1 end time"

label variable preg_num "Number of total pregnancy"
label variable c222_A "Pregnancy after the last one"
label variable c222_B "Pregnancy serial accuracy"
label variable c2000 "Section C2 end time"
label variable c244 "Presence of others during section C"
label variable c244_a "Type of person present"
label variable c244_a1 "Adult male (yes/no)"
label variable c244_a2 "Adult female (yes/no)"
label variable c244_a3 "Adolescent boy (yes/no)"
label variable c244_a4 "Adolescent girl (yes/no)"
label variable c244_a5 "Children (yes/no)"
label variable c244_a_1 "Adult male number"
label variable c244_a_2 "Adult female number"
label variable c244_a_3 "Adolescent boy number"
label variable c244_a_4 "Adolescent girl number"
label variable c244_a_5 "Children number"
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
label variable d10 "Other work during interview"
label variable d10_a "Type of other work"
label variable d10_a1 "Cooking"
label variable d10_a2 "Feeding HH members"
label variable d10_a3 "Feeding cattle/poultry"
label variable d10_a4 "Sewing"
label variable d10_a5 "Other HH chores"
label variable d10_a6 "Others"
label variable d10_a_1 "Specify HH chores"
label variable d10_a_2 "Specify others"
label variable d11 "Emotional breakdown"
label variable d11_a "Counselling"
label variable d12 "Discomfort (yes/no)"
label variable d12_a "Discomforting information"
label variable d12_a1 "Miscarriage information"
label variable d12_a2 "Abortion information"
label variable d12_a3 "Previous marriage information"
label variable d12_a4 "Other information"
label variable d12_a_1 "Specify other information"
label variable d13 "Seek help from other people"
label variable d13_a "Other people interfared"
label variable d14 "Respondent in urgency"
label variable d9 "Comments about the interview"
label variable d000 "Section D end time"

label variable dd "Interview end time"
***********************************************

*Replacing Variables

*First serial of the HH

replace serial1="1659" if _index==1
replace serial1="1657" if _index==2
replace serial1="1654" if _index==3
replace serial1="1655" if _index==4
replace serial1="1643" if _index==5
replace serial1="1642" if _index==6
replace serial1="1449" if _index==7
replace serial1="1451" if _index==8
replace serial1="1448" if _index==9
replace serial1="1446" if _index==10
replace serial1="1450" if _index==11
replace serial1="1447" if _index==12
replace serial1="1455" if _index==13
replace serial1="1457" if _index==14
replace serial1="1453" if _index==15
replace serial1="1458" if _index==16
replace serial1="1294" if _index==17
replace serial1="1295" if _index==18
replace serial1="1286" if _index==19
replace serial1="1285" if _index==20
replace serial1="1296" if _index==21
replace serial1="1292" if _index==22
replace serial1="1283" if _index==23
replace serial1="1288" if _index==24
replace serial1="1389" if _index==25
replace serial1="1388" if _index==26
replace serial1="1387" if _index==27
replace serial1="1395" if _index==28
replace serial1="1384" if _index==29
replace serial1="1382" if _index==30
replace serial1="1394" if _index==31
replace serial1="1396" if _index==32
replace serial1="1399" if _index==33
replace serial1="1390" if _index==34
replace serial1="1851" if _index==35
replace serial1="1854" if _index==36
replace serial1="1865" if _index==37
replace serial1="1870" if _index==38
replace serial1="1874" if _index==39
replace serial1="1863" if _index==40
replace serial1="1846" if _index==41
replace serial1="1867" if _index==42
replace serial1="1873" if _index==43
replace serial1="1872" if _index==44
replace serial1="1866" if _index==45
replace serial1="1877" if _index==46
replace serial1="1850" if _index==47
replace serial1="1849" if _index==48
replace serial1="1858" if _index==49
replace serial1="1859" if _index==50
replace serial1="1570" if _index==51
replace serial1="1572" if _index==52
replace serial1="1573" if _index==53
replace serial1="1304" if _index==54
replace serial1="1581" if _index==55
replace serial1="1302" if _index==56
replace serial1="1305" if _index==57
replace serial1="1862" if _index==58
replace serial1="1871" if _index==59
replace serial1="1848" if _index==60
replace serial1="1856" if _index==61
replace serial1="1852" if _index==62
replace serial1="1860" if _index==63
replace serial1="1861" if _index==64
replace serial1="1855" if _index==65
replace serial1="1426" if _index==66
replace serial1="1425" if _index==67
replace serial1="1429" if _index==68
replace serial1="1436" if _index==69
replace serial1="1417" if _index==70
replace serial1="1420" if _index==71
replace serial1="1413" if _index==72
replace serial1="1440" if _index==73
replace serial1="1441" if _index==74
replace serial1="1424" if _index==75
replace serial1="1439" if _index==76
replace serial1="1443" if _index==77
replace serial1="1416" if _index==78
replace serial1="1287" if _index==79
replace serial1="1291" if _index==80
replace serial1="1422" if _index==81
replace serial1="1434" if _index==82
replace serial1="1430" if _index==83
replace serial1="1431" if _index==84
replace serial1="1433" if _index==85
replace serial1="1432" if _index==86
replace serial1="1445" if _index==87
replace serial1="1444" if _index==88
replace serial1="1437" if _index==89
replace serial1="1428" if _index==90
replace serial1="1427" if _index==91
replace serial1="1435" if _index==92
replace serial1="1605" if _index==93
replace serial1="1623" if _index==94
replace serial1="1617" if _index==95
replace serial1="1603" if _index==96
replace serial1="1607" if _index==97
replace serial1="1609" if _index==98
replace serial1="1618" if _index==99
replace serial1="1622" if _index==100
replace serial1="1613" if _index==101
replace serial1="1614" if _index==102
replace serial1="1611" if _index==103
replace serial1="1612" if _index==104
replace serial1="1621" if _index==105
replace serial1="1619" if _index==106
replace serial1="1600" if _index==107
replace serial1="1615" if _index==108
replace serial1="1253" if _index==109
replace serial1="1268" if _index==110
replace serial1="1323" if _index==111
replace serial1="1324" if _index==112
replace serial1="1252" if _index==113
replace serial1="1266" if _index==114
replace serial1="1259" if _index==115
replace serial1="1257" if _index==116
replace serial1="1255" if _index==117
replace serial1="1260" if _index==118
replace serial1="1261" if _index==119
replace serial1="1262" if _index==120
replace serial1="1256" if _index==121
replace serial1="1258" if _index==122
replace serial1="1319" if _index==123
replace serial1="1320" if _index==124
replace serial1="1254" if _index==125
replace serial1="1264" if _index==126
replace serial1="1321" if _index==127
replace serial1="1267" if _index==128
replace serial1="1328" if _index==129
replace serial1="1329" if _index==130
replace serial1="1317" if _index==131
replace serial1="1318" if _index==132
replace serial1="1193" if _index==133
replace serial1="1196" if _index==134
replace serial1="1327" if _index==135
replace serial1="1332" if _index==136
replace serial1="1310" if _index==137
replace serial1="1311" if _index==138
replace serial1="1312" if _index==139
replace serial1="1313" if _index==140
replace serial1="1198" if _index==141
replace serial1="1192" if _index==142
replace serial1="1325" if _index==143
replace serial1="1333" if _index==144
replace serial1="1334" if _index==145
replace serial1="1316" if _index==146
replace serial1="1195" if _index==147
replace serial1="1191" if _index==148
replace serial1="1330" if _index==149
replace serial1="1326" if _index==150
replace serial1="1314" if _index==151
replace serial1="1307" if _index==152
replace serial1="1308" if _index==153
replace serial1="1197" if _index==154
replace serial1="1824" if _index==155
replace serial1="1817" if _index==156
replace serial1="1194" if _index==157
replace serial1="1811" if _index==158
replace serial1="1813" if _index==159
replace serial1="1806" if _index==160
replace serial1="1807" if _index==161
replace serial1="1808" if _index==162
replace serial1="1805" if _index==163
replace serial1="1804" if _index==164
replace serial1="1803" if _index==165
replace serial1="1810" if _index==166
replace serial1="1196" if _index==167
replace serial1="1816" if _index==168
replace serial1="1815" if _index==169
replace serial1="1198" if _index==170
replace serial1="1818" if _index==171
replace serial1="1812" if _index==172
replace serial1="1823" if _index==173
replace serial1="1809" if _index==174

replace serial1="1546" if _index==175
replace serial1="1555" if _index==176
replace serial1="1562" if _index==177
replace serial1="1547" if _index==178
replace serial1="1814" if _index==179
replace serial1="1549" if _index==180
replace serial1="1554" if _index==181
replace serial1="1559" if _index==182
replace serial1="1566" if _index==183
*replace serial1="" if _index==184
replace serial1="1550" if _index==185
replace serial1="1552" if _index==186
replace serial1="1553" if _index==187
replace serial1="1563" if _index==188
replace serial1="1564" if _index==189
replace serial1="1557" if _index==190
replace serial1="1556" if _index==191
replace serial1="1558" if _index==192
replace serial1="1560" if _index==193
replace serial1="1561" if _index==194
replace serial1="1565" if _index==195
replace serial1="1759" if _index==196
replace serial1="1761" if _index==197
replace serial1="1760" if _index==198
replace serial1="1775" if _index==199
replace serial1="1773" if _index==200
replace serial1="1769" if _index==201
replace serial1="1763" if _index==202
replace serial1="1765" if _index==203
replace serial1="1815" if _index==204
replace serial1="1820" if _index==205
replace serial1="1782" if _index==206
replace serial1="1778" if _index==207
replace serial1="1756" if _index==208
replace serial1="1757" if _index==209
replace serial1="1758" if _index==210
replace serial1="1764" if _index==211
replace serial1="1779" if _index==212
replace serial1="1772" if _index==213
replace serial1="1770" if _index==214
replace serial1="1398" if _index==215
replace serial1="1391" if _index==216
replace serial1="1386" if _index==217
replace serial1="1383" if _index==218
replace serial1="1869" if _index==219
replace serial1="1257" if _index==220
replace serial1="1571" if _index==221
replace serial1="1261" if _index==222
replace serial1="1414" if _index==223
replace serial1="1768" if _index==224
replace serial1="1777" if _index==225
replace serial1="1767" if _index==226
replace serial1="1766" if _index==227
replace serial1="1774" if _index==228
replace serial1="1583" if _index==229
replace serial1="1857" if _index==230
replace serial1="1853" if _index==231
replace serial1="1582" if _index==232
replace serial1="1579" if _index==233
replace serial1="1584" if _index==234
replace serial1="1303" if _index==235
replace serial1="1299" if _index==236
replace serial1="1353" if _index==237
replace serial1="1357" if _index==238
replace serial1="1356" if _index==239
replace serial1="1771" if _index==240
replace serial1="1753" if _index==241
replace serial1="1754" if _index==242
replace serial1="1073" if _index==243
replace serial1="1074" if _index==244
replace serial1="1076" if _index==245
replace serial1="1776" if _index==246
replace serial1="1780" if _index==247
replace serial1="1075" if _index==248
replace serial1="1758" if _index==249
replace serial1="1398" if _index==250
replace serial1="1721" if _index==251
replace serial1="1723" if _index==252
replace serial1="1724" if _index==253
replace serial1="1719" if _index==254
replace serial1="1718" if _index==255
replace serial1="1070" if _index==256
replace serial1="1072" if _index==257
replace serial1="1071" if _index==258
replace serial1="1084" if _index==259
replace serial1="1085" if _index==260
replace serial1="1133" if _index==261
replace serial1="1783" if _index==262
replace serial1="1081" if _index==263
replace serial1="1368" if _index==264
replace serial1="1372" if _index==265
replace serial1="1371" if _index==266
replace serial1="1374" if _index==267
replace serial1="1370" if _index==268
replace serial1="1375" if _index==269
replace serial1="1373" if _index==270
replace serial1="1648" if _index==271
replace serial1="1649" if _index==272
replace serial1="1300" if _index==273
replace serial1="1638" if _index==274
replace serial1="1647" if _index==275
replace serial1="1064" if _index==276
replace serial1="1065" if _index==277
replace serial1="1069" if _index==278
replace serial1="1068" if _index==279
replace serial1="1067" if _index==280
replace serial1="1653" if _index==281
replace serial1="1289" if _index==282
replace serial1="1456" if _index==283
replace serial1="1454" if _index==284
replace serial1="1459" if _index==285
replace serial1="1452" if _index==286
replace serial1="1284" if _index==287
replace serial1="1290" if _index==288
replace serial1="1297" if _index==289
replace serial1="1298" if _index==290
replace serial1="1293" if _index==291
replace serial1="1385" if _index==292
replace serial1="1393" if _index==293
replace serial1="1864" if _index==294
replace serial1="1868" if _index==295
replace serial1="1847" if _index==296
replace serial1="1876" if _index==297
replace serial1="1580" if _index==298
replace serial1="1585" if _index==299
replace serial1="1586" if _index==300
replace serial1="1875" if _index==301
replace serial1="1412" if _index==302
replace serial1="1411" if _index==303
replace serial1="1415" if _index==304
replace serial1="1421" if _index==305
replace serial1="1423" if _index==306
replace serial1="1410" if _index==307
replace serial1="1438" if _index==308
replace serial1="1601" if _index==309
replace serial1="1604" if _index==310
replace serial1="1610" if _index==311
replace serial1="1574" if _index==312
replace serial1="1616" if _index==313
replace serial1="1138" if _index==314
replace serial1="1135" if _index==315
replace serial1="1140" if _index==316
replace serial1="1731" if _index==317
replace serial1="1580" if _index==318
replace serial1="1656" if _index==319
replace serial1="1637" if _index==320
replace serial1="1662" if _index==321
replace serial1="1645" if _index==322
replace serial1="1289" if _index==323
replace serial1="1086" if _index==324
replace serial1="1066" if _index==325
replace serial1="1083" if _index==326
replace serial1="1367" if _index==327
replace serial1="1720" if _index==328
replace serial1="1369" if _index==329
replace serial1="1368" if _index==330
replace serial1="1370" if _index==331
replace serial1="1558" if _index==332
replace serial1="1556" if _index==333
replace serial1="1550" if _index==334
replace serial1="1551" if _index==335
replace serial1="1136" if _index==336
replace serial1="1137" if _index==337
replace serial1="1141" if _index==338
replace serial1="1142" if _index==339
replace serial1="1132" if _index==340
replace serial1="1636" if _index==341
replace serial1="1661" if _index==342
replace serial1="1646" if _index==343
replace serial1="1663" if _index==344
replace serial1="1665" if _index==345
replace serial1="1361" if _index==346
replace serial1="1362" if _index==347
replace serial1="1359" if _index==348
replace serial1="1360" if _index==349
replace serial1="1146" if _index==350
replace serial1="1147" if _index==351
replace serial1="1145" if _index==352

replace serial1="1163" if _index==393
replace serial1="1153" if _index==394
replace serial1="1160" if _index==395
replace serial1="1161" if _index==396
replace serial1="1162" if _index==397
replace serial1="1133" if _index==398
replace serial1="1477" if _index==399
replace serial1="1337" if _index==400
replace serial1="1474" if _index==401
replace serial1="1349" if _index==402
replace serial1="1338" if _index==403
replace serial1="1378" if _index==404
replace serial1="1381" if _index==405
replace serial1="1473" if _index==406
replace serial1="1479" if _index==407
replace serial1="1335" if _index==408
replace serial1="1380" if _index==409
replace serial1="1475" if _index==410
replace serial1="1478" if _index==411
replace serial1="1336" if _index==412

************************
replace serial1="1246" if _index==413
replace serial1="1243" if _index==414
replace serial1="1242" if _index==415
replace serial1="1251" if _index==416
replace serial1="1250" if _index==417
replace serial1="1241" if _index==418
replace serial1="1248" if _index==419
replace serial1="1249" if _index==420
replace serial1="1247" if _index==421
replace serial1="1276" if _index==422
replace serial1="1281" if _index==423
replace serial1="1099" if _index==424
replace serial1="1282" if _index==425
replace serial1="1102" if _index==426
replace serial1="1105" if _index==427
replace serial1="1278" if _index==428
replace serial1="1279" if _index==429
replace serial1="1104" if _index==430
replace serial1="1277" if _index==431
replace serial1="1274" if _index==432
replace serial1="1275" if _index==433
replace serial1="1101" if _index==434
replace serial1="1100" if _index==435
replace serial1="1005" if _index==436
replace serial1="1082" if _index==437
replace serial1="1548" if _index==438
replace serial1="1004" if _index==439
replace serial1="1392" if _index==440
replace serial1="1367" if _index==441
replace serial1="1397" if _index==442
replace serial1="1144" if _index==443
replace serial1="1143" if _index==444
replace serial1="1095" if _index==445
replace serial1="1094" if _index==446
replace serial1="1090" if _index==447
replace serial1="1057" if _index==448
replace serial1="1051" if _index==449
replace serial1="1093" if _index==450
replace serial1="1091" if _index==451
replace serial1="1092" if _index==452
replace serial1="1819" if _index==453
replace serial1="1802" if _index==454
replace serial1="1801" if _index==455
replace serial1="1793" if _index==456
replace serial1="1796" if _index==457
replace serial1="1786" if _index==458
replace serial1="1785" if _index==459
replace serial1="1800" if _index==460
replace serial1="1799" if _index==461
replace serial1="1794" if _index==462
replace serial1="1797" if _index==463
replace serial1="1792" if _index==464
replace serial1="1787" if _index==465
replace serial1="1784" if _index==466
replace serial1="1049" if _index==467
replace serial1="1046" if _index==468
replace serial1="1045" if _index==469
replace serial1="1039" if _index==470
replace serial1="1041" if _index==471
replace serial1="1048" if _index==472
replace serial1="1044" if _index==473
replace serial1="1047" if _index==474
replace serial1="1040" if _index==475
replace serial1="1042" if _index==476
replace serial1="1043" if _index==477
replace serial1="1788" if _index==478
replace serial1="1028" if _index==479
replace serial1="1027" if _index==480
replace serial1="1025" if _index==481
replace serial1="1024" if _index==482
replace serial1="1033" if _index==483
replace serial1="1036" if _index==484
replace serial1="1034" if _index==485
replace serial1="1035" if _index==486
replace serial1="1032" if _index==487
replace serial1="1037" if _index==488
replace serial1="1038" if _index==489
replace serial1="1465" if _index==490
replace serial1="1466" if _index==491
replace serial1="1469" if _index==492
replace serial1="1470" if _index==493
replace serial1="1467" if _index==494
replace serial1="1480" if _index==495
replace serial1="1483" if _index==496
replace serial1="1791" if _index==497
replace serial1="1790" if _index==498
replace serial1="1018" if _index==499
replace serial1="1031" if _index==500
replace serial1="1029" if _index==501
replace serial1="1023" if _index==502
replace serial1="1626" if _index==503
replace serial1="1020" if _index==504
replace serial1="1183" if _index==505
replace serial1="1627" if _index==506
replace serial1="1481" if _index==507
replace serial1="1482" if _index==508
replace serial1="1484" if _index==509
replace serial1="1464" if _index==510
replace serial1="1177" if _index==511
replace serial1="1181" if _index==512
replace serial1="1789" if _index==513
replace serial1="1022" if _index==514
replace serial1="1021" if _index==515
replace serial1="1041" if _index==516
replace serial1="1032" if _index==517
replace serial1="1628" if _index==518
replace serial1="1629" if _index==519
replace serial1="1625" if _index==520
replace serial1="1624" if _index==521
replace serial1="1019" if _index==522
replace serial1="1182" if _index==523
replace serial1="1534" if _index==524
replace serial1="1533" if _index==525
replace serial1="1541" if _index==526
replace serial1="1535" if _index==527
replace serial1="1540" if _index==528
replace serial1="1537" if _index==529
replace serial1="1530" if _index==530
replace serial1="1527" if _index==531
replace serial1="1528" if _index==532
replace serial1="1532" if _index==533
replace serial1="1531" if _index==534
replace serial1="1521" if _index==535
replace serial1="1520" if _index==536
replace serial1="1524" if _index==537
replace serial1="1522" if _index==538
replace serial1="1523" if _index==539
replace serial1="1526" if _index==540
replace serial1="1525" if _index==541
replace serial1="1529" if _index==542
replace serial1="1518" if _index==543
replace serial1="1519" if _index==544
replace serial1="1517" if _index==545
replace serial1="1536" if _index==546
replace serial1="1675" if _index==547
replace serial1="1674" if _index==548
replace serial1="1739" if _index==549
replace serial1="1538" if _index==550
replace serial1="1539" if _index==551
replace serial1="1667" if _index==552
replace serial1="1666" if _index==553
replace serial1="1670" if _index==554
replace serial1="1669" if _index==555
replace serial1="1673" if _index==556
replace serial1="1741" if _index==557
replace serial1="1738" if _index==558
replace serial1="1737" if _index==559
replace serial1="1490" if _index==560
replace serial1="1489" if _index==561
replace serial1="1672" if _index==562
replace serial1="1671" if _index==563
replace serial1="1740" if _index==564
replace serial1="1736" if _index==565
replace serial1="1486" if _index==566
replace serial1="1487" if _index==567
replace serial1="1885" if _index==568
replace serial1="1891" if _index==569
replace serial1="1886" if _index==570
replace serial1="1888" if _index==571
replace serial1="1892" if _index==572
replace serial1="1884" if _index==573
replace serial1="1699" if _index==574
replace serial1="1890" if _index==575
replace serial1="1887" if _index==576
replace serial1="1881" if _index==577
replace serial1="1703" if _index==578
replace serial1="1880" if _index==579
replace serial1="1882" if _index==580
replace serial1="1358" if _index==581
replace serial1="1697" if _index==582
replace serial1="1704" if _index==583
replace serial1="1879" if _index==584
replace serial1="1354" if _index==585
replace serial1="1878" if _index==586
replace serial1="1883" if _index==587
replace serial1="1353" if _index==588
replace serial1="1352" if _index==589
replace serial1="1702" if _index==590
replace serial1="1701" if _index==591
replace serial1="1052" if _index==592
replace serial1="1054" if _index==593
replace serial1="1050" if _index==594
replace serial1="1056" if _index==595
replace serial1="1059" if _index==596
replace serial1="1062" if _index==597
replace serial1="1243" if _index==598
replace serial1="1379" if _index==599
replace serial1="1327" if _index==600
replace serial1="1186" if _index==601
replace serial1="1185" if _index==602
replace serial1="1798" if _index==603
replace serial1="1049" if _index==604
replace serial1="1038" if _index==605
replace serial1="1792" if _index==606
replace serial1="1755" if _index==607
replace serial1="1400" if _index==608
replace serial1="1151" if _index==609
replace serial1="1150" if _index==610
replace serial1="1152" if _index==611
replace serial1="1408" if _index==612
replace serial1="1053" if _index==613
replace serial1="1055" if _index==614
replace serial1="1058" if _index==615
replace serial1="1060" if _index==616
replace serial1="1063" if _index==617
replace serial1="1077" if _index==618
replace serial1="1078" if _index==619

replace serial1="1492" if _index==723

*Second serial of the HH

replace serial2="iza" if _index==1
replace serial2="gza" if _index==2
replace serial2="dza" if _index==3
replace serial2="eza" if _index==4
replace serial2="sya" if _index==5
replace serial2="rya" if _index==6
replace serial2="gra" if _index==7
replace serial2="ira" if _index==8
replace serial2="fra" if _index==9
replace serial2="dra" if _index==10
replace serial2="hra" if _index==11
replace serial2="era" if _index==12
replace serial2="mra" if _index==13
replace serial2="ora" if _index==14
replace serial2="kra" if _index==15
replace serial2="pra" if _index==16
replace serial2="hla" if _index==17
replace serial2="ila" if _index==18
replace serial2="zka" if _index==19
replace serial2="yka" if _index==20
replace serial2="jla" if _index==21
replace serial2="fla" if _index==22
replace serial2="wka" if _index==23
replace serial2="bla" if _index==24
replace serial2="yoa" if _index==25
replace serial2="xoa" if _index==26
replace serial2="woa" if _index==27
replace serial2="epa" if _index==28
replace serial2="toa" if _index==29
replace serial2="roa" if _index==30
replace serial2="dpa" if _index==31
replace serial2="fpa" if _index==32
replace serial2="ipa" if _index==33
replace serial2="zoa" if _index==34
replace serial2="sgb" if _index==35
replace serial2="vgb" if _index==36
replace serial2="ghb" if _index==37
replace serial2="lhb" if _index==38
replace serial2="phb" if _index==39
replace serial2="ehb" if _index==40
replace serial2="ngb" if _index==41
replace serial2="ihb" if _index==42
replace serial2="ohb" if _index==43
replace serial2="nhb" if _index==44
replace serial2="hhb" if _index==45
replace serial2="shb" if _index==46
replace serial2="rgb" if _index==47
replace serial2="qgb" if _index==48
replace serial2="zgb" if _index==49
replace serial2="ahb" if _index==50
replace serial2="xva" if _index==51
replace serial2="zva" if _index==52
replace serial2="awa" if _index==53
replace serial2="rla" if _index==54
replace serial2="iwa" if _index==55
replace serial2="pla" if _index==56
replace serial2="sla" if _index==57
replace serial2="dhb" if _index==58
replace serial2="mhb" if _index==59
replace serial2="pgb" if _index==60
replace serial2="xgb" if _index==61
replace serial2="tgb" if _index==62
replace serial2="bhb" if _index==63
replace serial2="chb" if _index==64
replace serial2="wgb" if _index==65
replace serial2="jqa" if _index==66
replace serial2="iqa" if _index==67
replace serial2="mqa" if _index==68
replace serial2="tqa" if _index==69
replace serial2="aqa" if _index==70
replace serial2="dqa" if _index==71
replace serial2="wpa" if _index==72
replace serial2="xqa" if _index==73
replace serial2="yqa" if _index==74
replace serial2="hqa" if _index==75
replace serial2="wqa" if _index==76
replace serial2="ara" if _index==77
replace serial2="zpa" if _index==78
replace serial2="ala" if _index==79
replace serial2="ela" if _index==80
replace serial2="fqa" if _index==81
replace serial2="rqa" if _index==82
replace serial2="nqa" if _index==83
replace serial2="oqa" if _index==84
replace serial2="qqa" if _index==85
replace serial2="pqa" if _index==86
replace serial2="cra" if _index==87
replace serial2="bra" if _index==88
replace serial2="uqa" if _index==89
replace serial2="lqa" if _index==90
replace serial2="kqa" if _index==91
replace serial2="sqa" if _index==92
replace serial2="gxa" if _index==93
replace serial2="yxa" if _index==94
replace serial2="sxa" if _index==95
replace serial2="exa" if _index==96
replace serial2="ixa" if _index==97
replace serial2="kxa" if _index==98
replace serial2="txa" if _index==99
replace serial2="xxa" if _index==100
replace serial2="oxa" if _index==101
replace serial2="pxa" if _index==102
replace serial2="mxa" if _index==103
replace serial2="nxa" if _index==104
replace serial2="wxa" if _index==105
replace serial2="uxa" if _index==106
replace serial2="bxa" if _index==107
replace serial2="qxa" if _index==108
replace serial2="sja" if _index==109
replace serial2="hka" if _index==110
replace serial2="kma" if _index==111
replace serial2="lma" if _index==112
replace serial2="rja" if _index==113
replace serial2="fka" if _index==114
replace serial2="yja" if _index==115
replace serial2="wja" if _index==116
replace serial2="uja" if _index==117
replace serial2="zja" if _index==118
replace serial2="aka" if _index==119
replace serial2="bka" if _index==120
replace serial2="vja" if _index==121
replace serial2="xja" if _index==122
replace serial2="gma" if _index==123
replace serial2="hma" if _index==124
replace serial2="tja" if _index==125
replace serial2="dka" if _index==126
replace serial2="ima" if _index==127
replace serial2="gka" if _index==128
replace serial2="pma" if _index==129
replace serial2="qma" if _index==130
replace serial2="ema" if _index==131
replace serial2="fma" if _index==132
replace serial2="kha" if _index==133
replace serial2="nha" if _index==134
replace serial2="oma" if _index==135
replace serial2="tma" if _index==136
replace serial2="xla" if _index==137
replace serial2="yla" if _index==138
replace serial2="zla" if _index==139
replace serial2="ama" if _index==140
replace serial2="pha" if _index==141
replace serial2="jha" if _index==142
replace serial2="mma" if _index==143
replace serial2="uma" if _index==144
replace serial2="vma" if _index==145
replace serial2="dma" if _index==146
replace serial2="mha" if _index==147
replace serial2="iha" if _index==148
replace serial2="rma" if _index==149
replace serial2="nma" if _index==150
replace serial2="bma" if _index==151
replace serial2="ula" if _index==152
replace serial2="vla" if _index==153
replace serial2="oha" if _index==154
replace serial2="rfb" if _index==155
replace serial2="kfb" if _index==156
replace serial2="lha" if _index==157
replace serial2="efb" if _index==158
replace serial2="gfb" if _index==159
replace serial2="zeb" if _index==160
replace serial2="afb" if _index==161
replace serial2="bfb" if _index==162
replace serial2="yeb" if _index==163
replace serial2="xeb" if _index==164
replace serial2="web" if _index==165
replace serial2="dfb" if _index==166
replace serial2="nha" if _index==167
replace serial2="jfb" if _index==168
replace serial2="ifb" if _index==169
replace serial2="pha" if _index==170
replace serial2="lfb" if _index==171
replace serial2="ffb" if _index==172
replace serial2="qfb" if _index==173
replace serial2="cfb" if _index==174

replace serial2="zua" if _index==175
replace serial2="iva" if _index==176
replace serial2="pva" if _index==177
replace serial2="ava" if _index==178
replace serial2="hfb" if _index==179
replace serial2="cva" if _index==180
replace serial2="hva" if _index==181
replace serial2="mva" if _index==182
replace serial2="tva" if _index==183
*replace serial2="" if _index==184
replace serial2="dva" if _index==185
replace serial2="fva" if _index==186
replace serial2="gva" if _index==187
replace serial2="qva" if _index==188
replace serial2="rva" if _index==189
replace serial2="kva" if _index==190
replace serial2="jva" if _index==191
replace serial2="lva" if _index==192
replace serial2="nva" if _index==193
replace serial2="ova" if _index==194
replace serial2="sva" if _index==195
replace serial2="edb" if _index==196
replace serial2="gdb" if _index==197
replace serial2="fdb" if _index==198
replace serial2="udb" if _index==199
replace serial2="sdb" if _index==200
replace serial2="odb" if _index==201
replace serial2="idb" if _index==202
replace serial2="kdb" if _index==203
replace serial2="ifb" if _index==204
replace serial2="nfb" if _index==205
replace serial2="beb" if _index==206
replace serial2="xdb" if _index==207
replace serial2="bdb" if _index==208
replace serial2="cdb" if _index==209
replace serial2="ddb" if _index==210
replace serial2="jdb" if _index==211
replace serial2="ydb" if _index==212
replace serial2="rdb" if _index==213
replace serial2="pdb" if _index==214
replace serial2="hpa" if _index==215
replace serial2="apa" if _index==216
replace serial2="voa" if _index==217
replace serial2="soa" if _index==218
replace serial2="khb" if _index==219
replace serial2="wja" if _index==220
replace serial2="yva" if _index==221
replace serial2="aka" if _index==222
replace serial2="xpa" if _index==223
replace serial2="ndb" if _index==224
replace serial2="wdb" if _index==225
replace serial2="mdb" if _index==226
replace serial2="ldb" if _index==227
replace serial2="tdb" if _index==228
replace serial2="kwa" if _index==229
replace serial2="ygb" if _index==230
replace serial2="ugb" if _index==231
replace serial2="jwa" if _index==232
replace serial2="gwa" if _index==233
replace serial2="lwa" if _index==234
replace serial2="qla" if _index==235
replace serial2="mla" if _index==236
replace serial2="ona" if _index==237
replace serial2="sna" if _index==238
replace serial2="rna" if _index==239
replace serial2="qdb" if _index==240
replace serial2="ycb" if _index==241
replace serial2="zcb" if _index==242
replace serial2="uca" if _index==243
replace serial2="vca" if _index==244
replace serial2="xca" if _index==245
replace serial2="vdb" if _index==246
replace serial2="zdb" if _index==247
replace serial2="wca" if _index==248
replace serial2="ddb" if _index==249
replace serial2="hpa" if _index==250
replace serial2="sbb" if _index==251
replace serial2="ubb" if _index==252
replace serial2="vbb" if _index==253
replace serial2="qbb" if _index==254
replace serial2="pbb" if _index==255
replace serial2="rca" if _index==256
replace serial2="tca" if _index==257
replace serial2="sca" if _index==258
replace serial2="fda" if _index==259
replace serial2="gda" if _index==260
replace serial2="cfa" if _index==261
replace serial2="ceb" if _index==262
replace serial2="cda" if _index==263
replace serial2="doa" if _index==264
replace serial2="hoa" if _index==265
replace serial2="goa" if _index==266
replace serial2="joa" if _index==267
replace serial2="foa" if _index==268
replace serial2="koa" if _index==269
replace serial2="ioa" if _index==270
replace serial2="xya" if _index==271
replace serial2="yya" if _index==272
replace serial2="nla" if _index==273
replace serial2="nya" if _index==274
replace serial2="wya" if _index==275
replace serial2="lca" if _index==276
replace serial2="mca" if _index==277
replace serial2="qca" if _index==278
replace serial2="pca" if _index==279
replace serial2="oca" if _index==280
replace serial2="cza" if _index==281
replace serial2="cla" if _index==282
replace serial2="nra" if _index==283
replace serial2="lra" if _index==284
replace serial2="qra" if _index==285
replace serial2="jra" if _index==286
replace serial2="xka" if _index==287
replace serial2="dla" if _index==288
replace serial2="kla" if _index==289
replace serial2="lla" if _index==290
replace serial2="gla" if _index==291
replace serial2="uoa" if _index==292
replace serial2="cpa" if _index==293
replace serial2="fhb" if _index==294
replace serial2="jhb" if _index==295
replace serial2="ogb" if _index==296
replace serial2="rhb" if _index==297
replace serial2="hwa" if _index==298
replace serial2="mwa" if _index==299
replace serial2="nwa" if _index==300
replace serial2="qhb" if _index==301
replace serial2="vpa" if _index==302
replace serial2="upa" if _index==303
replace serial2="ypa" if _index==304
replace serial2="epa" if _index==305
replace serial2="gqa" if _index==306
replace serial2="tpa" if _index==307
replace serial2="vqa" if _index==308
replace serial2="cxa" if _index==309
replace serial2="fxa" if _index==310
replace serial2="lxa" if _index==311
replace serial2="bwa" if _index==312
replace serial2="rxa" if _index==313
replace serial2="hfa" if _index==314
replace serial2="efa" if _index==315
replace serial2="jfa" if _index==316
replace serial2="ccb" if _index==317
replace serial2="hwa" if _index==318
replace serial2="fza" if _index==319
replace serial2="mya" if _index==320
replace serial2="lza" if _index==321
replace serial2="uya" if _index==322
replace serial2="cla" if _index==323
replace serial2="hda" if _index==324
replace serial2="nca" if _index==325
replace serial2="eda" if _index==326
replace serial2="coa" if _index==327
replace serial2="rbb" if _index==328
replace serial2="eoa" if _index==329
replace serial2="doa" if _index==330
replace serial2="foa" if _index==331
replace serial2="lva" if _index==332
replace serial2="jva" if _index==333
replace serial2="dva" if _index==334
replace serial2="eva" if _index==335
replace serial2="ffa" if _index==336
replace serial2="gfa" if _index==337
replace serial2="kfa" if _index==338
replace serial2="lfa" if _index==339
replace serial2="bfa" if _index==340
replace serial2="lya" if _index==341
replace serial2="kza" if _index==342
replace serial2="vya" if _index==343
replace serial2="mza" if _index==344
replace serial2="oza" if _index==345
replace serial2="wna" if _index==346
replace serial2="xna" if _index==347
replace serial2="una" if _index==348
replace serial2="vna" if _index==349
replace serial2="pfa" if _index==350
replace serial2="qfa" if _index==351
replace serial2="ofa" if _index==352

replace serial2="gga" if _index==393
replace serial2="wfa" if _index==394
replace serial2="dga" if _index==395
replace serial2="ega" if _index==396
replace serial2="fga" if _index==397
replace serial2="cfa" if _index==398
replace serial2="isa" if _index==399
replace serial2="yma" if _index==400
replace serial2="fsa" if _index==401
replace serial2="kna" if _index==402
replace serial2="zma" if _index==403
replace serial2="noa" if _index==404
replace serial2="qoa" if _index==405
replace serial2="esa" if _index==406
replace serial2="ksa" if _index==407
replace serial2="wma" if _index==408
replace serial2="poa" if _index==409
replace serial2="gsa" if _index==410
replace serial2="jsa" if _index==411
replace serial2="xma" if _index==412

********************
replace serial2="lja" if _index==413
replace serial2="ija" if _index==414
replace serial2="hja" if _index==415
replace serial2="qja" if _index==416
replace serial2="pja" if _index==417
replace serial2="gja" if _index==418
replace serial2="nja" if _index==419
replace serial2="oja" if _index==420
replace serial2="mja" if _index==421
replace serial2="pka" if _index==422
replace serial2="uka" if _index==423
replace serial2="uda" if _index==424
replace serial2="vka" if _index==425
replace serial2="xda" if _index==426
replace serial2="aea" if _index==427
replace serial2="rka" if _index==428
replace serial2="ska" if _index==429
replace serial2="zda" if _index==430
replace serial2="qka" if _index==431
replace serial2="nka" if _index==432
replace serial2="oka" if _index==433
replace serial2="wda" if _index==434
replace serial2="vda" if _index==435
replace serial2="eaa" if _index==436
replace serial2="dda" if _index==437
replace serial2="bva" if _index==438
replace serial2="daa" if _index==439
replace serial2="bpa" if _index==440
replace serial2="coa" if _index==441
replace serial2="gpa" if _index==442
replace serial2="nfa" if _index==443
replace serial2="mfa" if _index==444
replace serial2="qda" if _index==445
replace serial2="pda" if _index==446
replace serial2="lda" if _index==447
replace serial2="eca" if _index==448
replace serial2="yba" if _index==449
replace serial2="oda" if _index==450
replace serial2="mda" if _index==451
replace serial2="nda" if _index==452
replace serial2="mfb" if _index==453
replace serial2="veb" if _index==454
replace serial2="ueb" if _index==455
replace serial2="meb" if _index==456
replace serial2="peb" if _index==457
replace serial2="feb" if _index==458
replace serial2="eeb" if _index==459
replace serial2="teb" if _index==460
replace serial2="seb" if _index==461
replace serial2="neb" if _index==462
replace serial2="qeb" if _index==463
replace serial2="leb" if _index==464
replace serial2="geb" if _index==465
replace serial2="deb" if _index==466
replace serial2="wba" if _index==467
replace serial2="tba" if _index==468
replace serial2="sba" if _index==469
replace serial2="mba" if _index==470
replace serial2="oba" if _index==471
replace serial2="vba" if _index==472
replace serial2="rba" if _index==473
replace serial2="uba" if _index==474
replace serial2="nba" if _index==475
replace serial2="pba" if _index==476
replace serial2="qba" if _index==477
replace serial2="heb" if _index==478
replace serial2="bba" if _index==479
replace serial2="aba" if _index==480
replace serial2="yaa" if _index==481
replace serial2="xaa" if _index==482
replace serial2="gba" if _index==483
replace serial2="jba" if _index==484
replace serial2="hba" if _index==485
replace serial2="iba" if _index==486
replace serial2="fba" if _index==487
replace serial2="kba" if _index==488
replace serial2="lba" if _index==489
replace serial2="wra" if _index==490
replace serial2="xra" if _index==491
replace serial2="asa" if _index==492
replace serial2="bsa" if _index==493
replace serial2="yra" if _index==494
replace serial2="lsa" if _index==495
replace serial2="osa" if _index==496
replace serial2="keb" if _index==497
replace serial2="jeb" if _index==498
replace serial2="raa" if _index==499
replace serial2="eba" if _index==500
replace serial2="cba" if _index==501
replace serial2="waa" if _index==502
replace serial2="bya" if _index==503
replace serial2="taa" if _index==504
replace serial2="aha" if _index==505
replace serial2="cya" if _index==506
replace serial2="msa" if _index==507
replace serial2="nsa" if _index==508
replace serial2="psa" if _index==509
replace serial2="vra" if _index==510
replace serial2="uga" if _index==511
replace serial2="yga" if _index==512
replace serial2="ieb" if _index==513
replace serial2="vaa" if _index==514
replace serial2="uaa" if _index==515
replace serial2="oba" if _index==516
replace serial2="fba" if _index==517
replace serial2="dya" if _index==518
replace serial2="eya" if _index==519
replace serial2="aya" if _index==520
replace serial2="zxa" if _index==521
replace serial2="saa" if _index==522
replace serial2="zga" if _index==523
replace serial2="nua" if _index==524
replace serial2="mua" if _index==525
replace serial2="uua" if _index==526
replace serial2="oua" if _index==527
replace serial2="tua" if _index==528
replace serial2="qua" if _index==529
replace serial2="jua" if _index==530
replace serial2="gua" if _index==531
replace serial2="hua" if _index==532
replace serial2="lua" if _index==533
replace serial2="kua" if _index==534
replace serial2="aua" if _index==535
replace serial2="zta" if _index==536
replace serial2="dua" if _index==537
replace serial2="bua" if _index==538
replace serial2="cua" if _index==539
replace serial2="fua" if _index==540
replace serial2="eua" if _index==541
replace serial2="iua" if _index==542
replace serial2="xta" if _index==543
replace serial2="yta" if _index==544
replace serial2="wta" if _index==545
replace serial2="pua" if _index==546
replace serial2="yza" if _index==547
replace serial2="xza" if _index==548
replace serial2="kcb" if _index==549
replace serial2="rua" if _index==550
replace serial2="sua" if _index==551
replace serial2="qza" if _index==552
replace serial2="pza" if _index==553
replace serial2="tza" if _index==554
replace serial2="sza" if _index==555
replace serial2="wza" if _index==556
replace serial2="mcb" if _index==557
replace serial2="jcb" if _index==558
replace serial2="icb" if _index==559
replace serial2="vsa" if _index==560
replace serial2="usa" if _index==561
replace serial2="vza" if _index==562
replace serial2="uza" if _index==563
replace serial2="lcb" if _index==564
replace serial2="hcb" if _index==565
replace serial2="rsa" if _index==566
replace serial2="ssa" if _index==567
replace serial2="aib" if _index==568
replace serial2="gib" if _index==569
replace serial2="bib" if _index==570
replace serial2="dib" if _index==571
replace serial2="hib" if _index==572
replace serial2="zhb" if _index==573
replace serial2="wab" if _index==574
replace serial2="fib" if _index==575
replace serial2="cib" if _index==576
replace serial2="whb" if _index==577
replace serial2="abb" if _index==578
replace serial2="vhb" if _index==579
replace serial2="xhb" if _index==580
replace serial2="tna" if _index==581
replace serial2="uab" if _index==582
replace serial2="bbb" if _index==583
replace serial2="uhb" if _index==584
replace serial2="pna" if _index==585
replace serial2="thb" if _index==586
replace serial2="yhb" if _index==587
replace serial2="ona" if _index==588
replace serial2="nna" if _index==589
replace serial2="zab" if _index==590
replace serial2="yab" if _index==591
replace serial2="zba" if _index==592
replace serial2="bca" if _index==593
replace serial2="xba" if _index==594
replace serial2="dca" if _index==595
replace serial2="gca" if _index==596
replace serial2="jca" if _index==597
replace serial2="ija" if _index==598
replace serial2="ooa" if _index==599
replace serial2="oma" if _index==600
replace serial2="dha" if _index==601
replace serial2="cha" if _index==602
replace serial2="reb" if _index==603
replace serial2="wba" if _index==604
replace serial2="lba" if _index==605
replace serial2="leb" if _index==606
replace serial2="adb" if _index==607
replace serial2="jpa" if _index==608
replace serial2="ufa" if _index==609
replace serial2="tfa" if _index==610
replace serial2="vfa" if _index==611
replace serial2="rpa" if _index==612
replace serial2="aca" if _index==613
replace serial2="cca" if _index==614
replace serial2="fca" if _index==615
replace serial2="hca" if _index==616
replace serial2="kca" if _index==617
replace serial2="yca" if _index==618
replace serial2="zca" if _index==619

replace serial2="xsa" if _index==723

*Third serial of the HH (s3)

gen combine= serial1 + serial2

replace s3=combine if _index<620
replace s3=combine if _index==723

order combine, after (s3)

*Third serial of the HH (s3)

replace serial3=combine if _index<620
replace serial3=combine if _index==723

drop combine

*Respondent's HH visiting number

replace x0="1" if _index<109

replace x0="1" if _index==161

*First visit date

replace x0_1 = date("07dec2024", "DMY") if _index<7

replace x0_1 = date("10dec2024", "DMY") if _index>6 & _index<17

replace x0_1 = date("11dec2024", "DMY") if _index>16 & _index<25

replace x0_1 = date("12dec2024", "DMY") if _index>24 & _index<35

replace x0_1 = date("14dec2024", "DMY") if _index>34 & _index<51

replace x0_1 = date("15dec2024", "DMY") if _index>50 & _index<66

replace x0_1 = date("17dec2024", "DMY") if _index>65 & _index<81

replace x0_1 = date("18dec2024", "DMY") if _index>80 & _index<93

replace x0_1 = date("19dec2024", "DMY") if _index>92 & _index<109

replace x0_1 = date("21dec2024", "DMY") if _index>108 & _index<129

replace x0_1 = date("22dec2024", "DMY") if _index>128 & _index<154

replace x0_1 = date("23dec2024", "DMY") if _index>153 & _index<167
replace x0_1 = date("23dec2024", "DMY") if _index==168
replace x0_1 = date("23dec2024", "DMY") if _index==169
replace x0_1 = date("23dec2024", "DMY") if _index==171
replace x0_1 = date("23dec2024", "DMY") if _index==172
replace x0_1 = date("23dec2024", "DMY") if _index==173
replace x0_1 = date("23dec2024", "DMY") if _index==174


*Second visit date

replace x0_2 = date("23dec2024", "DMY") if _index==167
replace x0_2 = date("23dec2024", "DMY") if _index==170


*Third visit date

*No third visit has been done untill 23-12-2024

*Interview status

replace x1="1" if _index<109

*Respondent's Name

replace a0_1="Akhi" if _index==1
replace a0_1="Shahinoor" if _index==2
replace a0_1="Shimla" if _index==3
replace a0_1="Ayesha Akter/ Bithi" if _index==4
replace a0_1="Eti Rani" if _index==5
replace a0_1="Peara" if _index==6
replace a0_1="Moyna" if _index==7
replace a0_1="Munni" if _index==8
replace a0_1="Shima" if _index==9
replace a0_1="Yeasmin Akter" if _index==10
replace a0_1="Shahinoor" if _index==11
replace a0_1="Jahanara" if _index==12
replace a0_1="Hazera" if _index==13
replace a0_1="Akhi" if _index==14
replace a0_1="Sonia" if _index==15
replace a0_1="Sania" if _index==16
replace a0_1="Rupaly" if _index==17
replace a0_1="Lovely" if _index==18
replace a0_1="Rashida" if _index==19
replace a0_1="Rahima" if _index==20
replace a0_1="Salina Akter (Kazal)" if _index==21
replace a0_1="Bulu Akter" if _index==22
replace a0_1="Ayesha" if _index==23
replace a0_1="Ruzina" if _index==24
replace a0_1="Shila" if _index==25
replace a0_1="Brithi Rani" if _index==26
replace a0_1="Sarmin" if _index==27
replace a0_1="Mahinoor" if _index==28
replace a0_1="Rumi (Shanu)" if _index==29
replace a0_1="Fatema" if _index==30
replace a0_1="Anjali" if _index==31
replace a0_1="Lucky" if _index==32
replace a0_1="Jannatul Ferdousi" if _index==33
replace a0_1="Pipasha" if _index==34

***********************************************
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

rename a71 w4

rename a72 w5

rename a73 w6

rename a74 w7

rename a75 w8

rename a76 w9

rename a77 w10

rename a78 w11

rename a79 w12

rename a710 w13

rename a711 w14

rename a712 w15

rename a713 w16

rename a714 w17

rename a715 w18

rename a716 w19

rename a717 w20

rename a718 w21

rename a719 w22

rename a720 w23

rename a721 w24

rename a722 w25

rename a723 w26

rename a724 w27

rename a725 w28

rename a726 w29

rename a727 w30

rename a728 w31

rename a729 w32

*Asset quintile
order w1 w2 w3, before (w4)
factor w1 - w32, pcf
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

*Experienced stillbirth, miscarriage, abortion (yes/no)

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

*Anyone else nearby during the interview of "section C"

destring c244, replace
label def c244 1"Full time" 2"Partial" 3"No one"
label val c244 c244

*Adult male present during the interview of "section C"

destring c244_a1, replace
label def c244_a1 1"Yes" 2"No"
label val c244_a1 c244_a1

*Adult female present during the interview of "section C"

destring c244_a2, replace
label def c244_a2 1"Yes" 2"No"
label val c244_a2 c244_a2

*Adolescent boy present during the interview of "section C"

destring c244_a3, replace
label def c244_a3 1"Yes" 2"No"
label val c244_a3 c244_a3

*Adolescent girl present during the interview of "section C"

destring c244_a4, replace
label def c244_a4 1"Yes" 2"No"
label val c244_a4 c244_a4

*Children present during the interview of "section C"

destring c244_a5, replace
label def c244_a5 1"Yes" 2"No"
label val c244_a5 c244_a5

***********************************************

*Section D: Interviewers observation

*Interview taken inside or outside the house

destring d1, replace
label def d1 1"Inside a room" 2"In the house" 2"Outside of the house"
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

*Respondent's involvement in other work during the interview

destring d10, replace
label def d10 1"Yes" 2"No"
label val d10 d10

*Type of other work during the interview

destring d10_a, replace
label def d10_a 1"Cooking" 2"Feeding HH members" 3"Feeding cattle/poultry" 4"Sewing" 5"Other HH chores" 6"Others"
label val d10_a d10_a

*Cooking during the interview

destring d10_a1, replace
label def d10_a1 1"Yes" 2"No"
label val d10_a1 d10_a1

*Feeding HH members during the interview

destring d10_a2, replace
label def d10_a2 1"Yes" 2"No"
label val d10_a2 d10_a2

*Feeding cattle/poultry during the interview

destring d10_a3, replace
label def d10_a3 1"Yes" 2"No"
label val d10_a3 d10_a3

*Sewing during the interview

destring d10_a4, replace
label def d10_a4 1"Yes" 2"No"
label val d10_a4 d10_a4

*Other HH chores during the interview

destring d10_a5, replace
label def d10_a5 1"Yes" 2"No"
label val d10_a5 d10_a5

*Other works during the interview

destring d10_a6, replace
label def d10_a6 1"Yes" 2"No"
label val d10_a6 d10_a6

*Experienced emotional breakdown during the interview

destring d11, replace
label def d11 1"Yes" 2"No"
label val d11 d11

*Counselling for the emotional breakdown during the interview

destring d11_a, replace
label def d11_a 1"Yes" 2"No"
label val d11_a d11_a

*Respondent's discomfort to provide any information during the interview

destring d12, replace
label def d12 1"Yes" 2"No"
label val d12 d12

*Types of information for which respondents felt discomfort

destring d12_a, replace
label def d12_a 1"Miscarriage related information" 2"Abortion related information" 3"Information related to previous marriage" 4"Others"
label val d12_a d12_a

*Respondent's discomfort to provide miscarriage related information

destring d12_a1, replace
label def d12_a1 1"Yes" 2"No"
label val d12_a1 d12_a1

*Respondent's discomfort to provide abortion related information

destring d12_a2, replace
label def d12_a2 1"Yes" 2"No"
label val d12_a2 d12_a2

*Respondent's discomfort to provide information related to previous marriage

destring d12_a3, replace
label def d12_a3 1"Yes" 2"No"
label val d12_a3 d12_a3

*Respondent's discomfort to provide other types of information

destring d12_a4, replace
label def d12_a4 1"Yes" 2"No"
label val d12_a4 d12_a4

*Respondent seek help from other people to provide information during the interview

destring d13, replace
label def d13 1"Yes" 2"No"
label val d13 d13

*Other people provide information spontaneously during the interview

destring d13_a, replace
label def d13_a 1"Yes" 2"No"
label val d13_a d13_a

*Respondent in urgency to do something or to go somewhere during the interview

destring d14, replace
label def d14 1"Yes" 2"No"
label val d14 d14

***********************************************

*Saving parent file

save final_parent.dta, replace

***********************************************
***********************************************

import excel "aXmwdKgwzBQDSJH6mm7jjM_2025_03_24_04_09_34.xlsx", sheet("Reproduction_section_Pregnancy_") firstrow clear //Change the excel file name 

***********************************************

*Dropping dummy data

drop if _parent_index>352 & _parent_index<393

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
rename AC c228_B

***********************************************

*Labeling the variables

label variable c200 "Section C2 start time"
label variable c215_a "Pregnancy serial number"
label variable c215 "Pregnancy type"
label variable c216 "Pregnancy outcome type"
label variable c216_a "Pregnancy outcome type 2"
label variable cal_216 "Pregnancy outcome type (calculate)"
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
label variable c223 "Pregnancy outcome type (calculate)"
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
label variable c228_B "Age (month) at death"

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

*Using Final Parent File

use final_parent.dta, clear

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

tab x1_a

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

*Total child born alive

tab c208

*Total live birth confirmation (yes/no)

tab c209

*Experienced stillbirth, miscarriage, abortion (yes/no)

tab c210

*Number of stillbirth, miscarriage, abortion

tab c211

*Total number of pregnancy of the respondent

tab c212

*Total number of pregnancy confirmation (yes/no)

tab c212_1

*Section C1000 end time

tab c1000

***********************************************

*Pregnancy after the last pregnancy

tab c222_A

*Checking the serial of the pregnancy

tab c222_B

*Section C2000 end time

tab c2000

***********************************************

*Anyone else nearby during the interview of "section C"

tab c244

*Type of the person present during the interview of Section-C

tab c244_a

*Adult male present during the interview of "section C"

tab c244_a1

tab c244_a_1

*Adult female present during the interview of "section C"

tab c244_a2

tab c244_a_2

*Adolescent boy present during the interview of "section C"

tab c244_a3

tab c244_a_3

*Adolescent girl present during the interview of "section C"

tab c244_a4

tab c244_a_4

*Children present during the interview of "section C"

tab c244_a5

tab c244_a_5

*Section C3000 end time

tab c3000

***********************************************

*Section D: Interviewers observation

*Section d00 start time

tab d00

*Place of the interview

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

*Respondent's involvement in other work during the interview

tab d10

*Type of other work during the interview

tab d10_a

*Cooking during the interview

tab d10_a1

*Feeding HH members during the interview

tab d10_a2

*Feeding cattle/poultry during the interview

tab d10_a3

*Sewing during the interview

tab d10_a4

*Other HH chores during the interview

tab d10_a5

tab d10_a_1

*Other works during the interview

tab d10_a6

tab d10_a_2

*Experienced emotional breakdown during the interview

tab d11

*Counselling for the emotional breakdown during the interview

tab d11_a

*Respondent's discomfort to provide any information during the interview

tab d12

*Types of information for which respondents felt discomfort

tab d12_a

*Respondent's discomfort to provide miscarriage related information

tab d12_a1

*Respondent's discomfort to provide abortion related information

tab d12_a2

*Respondent's discomfort to provide information related to previous marriage

tab d12_a3

*Respondent's discomfort to provide other types of information

tab d12_a4

tab d12_a_1

*Respondent seek help from other people to provide information during the interview

tab d13

*Other people provide information spontaneously during the interview

tab d13_a

*Respondent in urgency to do something or to go somewhere during the interview

tab d14

*Any other comments related to the interview

tab d9

*Section d000 end time

tab d000

***********************************************

*Interview end time

tab dd

***********************************************
***********************************************
*Section C.a: Pregnancy history

*Using Final Reproductive file

use final_rh.dta, clear

*Section C200 start time

tab c200

*Serial number of the pregnancy

tab c215_a

*Type of the pregnancy (single/ twin/ triple/ more)

tab c215

*Type of the pregnancy outcome

tab c216

tab c216_a

*Calculation of c216 (Type of the pregnancy outcome)

tab cal_216

*Move/ cry of the born dead child

tab c217

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
tab c223, nolabel

tab cal_223
tab cal_223, nolabel

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

*Son/ daughters age at death (in month) who born alive (category)

tab c228_B

***********************************************
***********************************************










