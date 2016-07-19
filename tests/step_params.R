library(adehabitatLT)
library(rpostgisLT)
data(ibexraw)

# ltraj with parameters not recomputed
#ib_df <- rpostgisLT:::ld_opt(ibexraw)[1:10, ]
#ib <- rpostgisLT:::dl_opt(ib_df)

# ltraj with recomputed parameters
ib_rec <- dl(ld(ibexraw[1])[1:10, ])

#        x       y                date   dx   dy       dist    dt      R2n
#85 897168 2030489 2003-06-01 00:00:56  -34  -23   41.04875 14396        0
#86 897134 2030466 2003-06-01 04:00:52  102 1128 1132.60231 28811     1685
#87 897236 2031594 2003-06-01 12:01:03   80  -31   85.79627 28830  1225649
#88 897316 2031563 2003-06-01 20:01:33   18 -244  244.66303 14352  1175380
#89 897334 2031319 2003-06-02 00:00:45   -1   -5    5.09902 14453   716456
#90 897333 2031314 2003-06-02 04:01:38  567 1853 1937.80752 14378   707850
#91 897900 2033167 2003-06-02 08:01:16 1790 1290 2206.39978 14404  7707508
#92 899690 2034457 2003-06-02 12:01:20  756  587  957.13374 14369 22105508
#93 900446 2035044 2003-06-02 16:00:49  313   44  316.07752 57633 31493309
#94 900759 2035088 2003-06-03 08:01:22   NA   NA         NA    NA 34046082
#    abs.angle   rel.angle
#85 -2.5468334          NA
#86  1.4806161 -2.25573586
#87 -0.3696843 -1.85030034
#88 -1.4971592 -1.12747493
#89 -1.7681919 -0.27103267
#90  1.2738530  3.04204484
#91  0.6244644 -0.64938853
#92  0.6602185  0.03575404
#93  0.1396599 -0.52055854
#94         NA          NA

# Insert into temporary table only
pgTrajSchema(conn, name = "params_test")
rpostgisLT:::pgTrajTempT(conn, "params_test")
rpostgisLT:::pgTrajR2TempT(conn, schema = "params_test", dframe = ib_df, 
        pgtraj = "ibexraw", epsg = 0)
# Insert the ltraj completely
ltraj2pgtraj(conn, schema = "params_test", ltraj = ib)
ltraj2pgtraj(conn, schema = "traj_t1", ltraj = ib_rec)
ltraj2pgtraj(conn, schema = "params_test2", ltraj = ib_rec)
ltraj2pgtraj(conn, schema = "params_test3", ltraj = ib_rec)

#######
# 1. Coordinates

#SELECT
#ST_x(relocation) AS x, 
#ST_y(relocation) AS y
#FROM params_test.relocs_temp;

#x           y
#897168.0	2030489.0
#897134.0	2030466.0
#897236.0	2031594.0
#897316.0	2031563.0
#897334.0	2031319.0
#897333.0	2031314.0
#897900.0	2033167.0
#899690.0	2034457.0
#900446.0	2035044.0
#900759.0	2035088.0

# This is correct.

########
# 2. Time zone - give the offset in hours relative to UTC
#SELECT extract(timezone FROM 
#(SELECT date FROM params_test.relocs_temp LIMIT 1))/3600.0 AS offset_from_UTC;

#offset_from_utc |
#----------------|
#2.0             |

# TODO Which is my local time zone...

#date                |
#--------------------|
#2003-06-01 00:00:56 |
#2003-06-01 04:00:52 |
#2003-06-01 12:01:03 |
#2003-06-01 20:01:33 |
#2003-06-02 00:00:45 |
#2003-06-02 04:01:38 |
#2003-06-02 08:01:16 |
#2003-06-02 12:01:20 |
#2003-06-02 16:00:49 |
#2003-06-03 08:01:22 |

# But the dates are correct.

########
# 3. dx, dy

# as.ltraj calculates it as 
#dx <- c(x1$x - x2$x, NA)
#dy <- c(x1$y - x2$y, NA)

#SELECT
#ST_Distance(
#        ST_Startpoint(s.step),
#        ST_SetSRID(
#                ST_Makepoint(
#                        ST_X(ST_endpoint(s.step)), 
#                        ST_Y(ST_startpoint(s.step))
#                ),
#                0
#        )
#) AS dx,
#ST_Distance(
#        ST_SetSRID(
#                ST_Makepoint(
#                        ST_x(ST_endpoint(s.step)), 
#                        ST_y(ST_startpoint(s.step))
#                ),
#                0
#        ),
#        ST_endpoint(s.step)
#) AS dy
#FROM params_test.steps s

#dx     |dy     |
#-------|-------|
#34.0   |23.0   |
#102.0  |1128.0 |
#80.0   |31.0   |
#18.0   |244.0  |
#1.0    |5.0    |
#567.0  |1853.0 |
#1790.0 |1290.0 |
#756.0  |587.0  |
#313.0  |44.0   |

#> ib_rec[[1]][,c("dx", "dy")]
#     dx   dy
#85  -34  -23
#86  102 1128
#87   80  -31
#88   18 -244
#89   -1   -5
#90  567 1853
#91 1790 1290
#92  756  587
#93  313   44
#94   NA   NA


# TODO The last record is missing from the 'steps' table, which is NA in the ltraj
# TODO Seems like that I confused which point to take first to calculate dx, dy

########
# 4. dist

# as.ltraj calculates is as
# dist <- c(sqrt((x1$x - x2$x)^2 + (x1$y - x2$y)^2),NA)

#SELECT 
#ST_length(s.step) AS dist
#FROM params_test.steps s;

#dist               |
#-------------------|
#41.048751503547585 |
#1132.60231325916   |
#85.7962703152066   |
#244.6630335788388  |
#5.0990195135927845 |
#1937.8075239816776 |
#2206.3997824510407 |
#957.133741960861   |
#316.07752213657966 |

#> ib_rec[[1]][,"dist",drop=FALSE]
#         dist
#85   41.04875
#86 1132.60231
#87   85.79627
#88  244.66303
#89    5.09902
#90 1937.80752
#91 2206.39978
#92  957.13374
#93  316.07752
#94         NA

# TODO Results are correct, but rounded to 5 decimals in an ltraj, compare:
# 5.0990195135927845 (pg) vs. 5.09902 (R)

############
# 5. dt

#SELECT extract(epoch FROM dt)
#FROM params_test.steps s;

#date_part |
#----------|
#14396.0   |
#28811.0   |
#28830.0   |
#14352.0   |
#14453.0   |
#14378.0   |
#14404.0   |
#14369.0   |
#57633.0   |

#> ib_rec[[1]][,"dt",drop=FALSE]
#      dt
#85 14396
#86 28811
#87 28830
#88 14352
#89 14453
#90 14378
#91 14404
#92 14369
#93 57633
#94    NA

# This is correct.

#############
# 6. R2n

# ltraj computes as 
# R2n <- (x$x - x$x[1])^2 + (x$y - x$y[1])^2

# select r2n from the parameters view
# SELECT r2n FROM params_test.ib_params;

# ...ST_Distance(startp.reloc1, s.reloc1) AS r2n,...

#r2n                |
#-------------------|
#0.0                |
#41.048751503547585 |
#1107.0903305512156 |
#1084.149436194107  |
#846.4372392564023  |
#841.3382197428095  |
#2776.239903178398  |
#4701.649497782667  |

#> ib_rec[[1]][,"R2n",drop=FALSE]
#        R2n
#85        0
#86     1685
#87  1225649
#88  1175380
#89   716456
#90   707850
#91  7707508
#92 22105508
#93 31493309
#94 34046082

# TODO in the parameters view there are only 8 records left from 10
# TODO the results are completely different, but the 'dist' parameter
# gives the same results, thus the error is probably due to a relocation mismatch

##############
# 7. abs.angle

# ltraj computes as
# abs.angle <- ifelse(dist<1e-07,NA,atan2(dy,dx))

# SELECT abs_angle FROM params_test.ib_params;

# ...atan2(t.dy, t.dx) AS abs_angle,...

#abs_angle           |
#--------------------|
#0.5947592574833528  |
#1.4806160558643302  |
#0.36968428879530135 |
#1.497159221494931   |
#1.373400766945016   |
#1.2738529579408087  |
#0.6244644241412693  |
#0.6602184645567633  |

#> ib_rec[[1]][,"abs.angle",drop=FALSE]
#    abs.angle
#85 -2.5468334
#86  1.4806161
#87 -0.3696843
#88 -1.4971592
#89 -1.7681919
#90  1.2738530
#91  0.6244644
#92  0.6602185
#93  0.1396599
#94         NA

# The majority of the values are correct
# TODO apart from the diffs. in sign, some records are incorrect (85, 89)

############
# 8. rel.angle

# SELECT rel_angle FROM params_test.ib_params;

# ...(
#        ST_Azimuth(ST_startpoint(s2.step), ST_endpoint(s2.step)) -
#        ST_Azimuth(ST_startpoint(s.step), ST_endpoint(s.step))
#    ) AS rel_angle,...

#rel_angle             |
#----------------------|
#-4.027449451970771    |
#1.8503003446596316    |
#1.1274749326996294    |
#0.2710326651498467    |
#-3.042044844585586    |
#0.6493885337995393    |
#-0.035754040415494015 |
#0.5205585426858099    |

#> ib_rec[[1]][,"rel.angle",drop=FALSE]
#     rel.angle
#85          NA
#86 -2.25573586
#87 -1.85030034
#88 -1.12747493
#89 -0.27103267
#90  3.04204484
#91 -0.64938853
#92  0.03575404
#93 -0.52055854
#94          NA

# The majority of the values are correct
# TODO first and last value missing
# TODO diffs. in some signs
