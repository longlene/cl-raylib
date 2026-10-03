(in-package #:cl-raylib)

;;;===================================================================================
;;; dr_mp3 - MP3 audio decoder (based on minimp3)
;;; Port of raylib/src/external/dr_mp3.h (v0.7.x), used by raudio
;;;
;;; Ported: minimp3 Layer I/II/III frame decoder (drmp3dec), memory streams with ID3v1,
;;; ID3v2, APE and Xing/Info/LAME tag handling, s16/f32 reading, brute force seeking
;;; and PCM frame counting
;;; NOTE: Matches the x86-64 SSE build of dr_mp3: DCT-II sums use the SIMD association
;;; and synthesis output uses round-to-nearest-even conversion (_mm_cvtps_epi32)
;;; NOTE: Data is always decoded from memory (files are loaded to memory first)
;;;===================================================================================

(defconstant +drmp3-max-pcm-frames-per-mp3-frame+ 1152)
(defconstant +drmp3-max-samples-per-frame+ (* +drmp3-max-pcm-frames-per-mp3-frame+ 2))
(defconstant +drmp3-max-bitreservoir-bytes+ 511)
(defconstant +drmp3-max-free-format-frame-size+ 2304)
(defconstant +drmp3-max-l3-frame-payload-bytes+ +drmp3-max-free-format-frame-size+)
(defconstant +drmp3-max-frame-sync-matches+ 10)
(defconstant +drmp3-short-block-type+ 2)
(defconstant +drmp3-stop-block-type+ 3)
(defconstant +drmp3-mode-mono+ 3)
(defconstant +drmp3-mode-joint-stereo+ 1)
(defconstant +drmp3-hdr-size+ 4)
(defconstant +drmp3-bits-dequantizer-out+ -1)
(defconstant +drmp3-max-scf+ (+ 255 (* +drmp3-bits-dequantizer-out+ 4) -210))
(defconstant +drmp3-max-scfi+ (logand (+ +drmp3-max-scf+ 3) (lognot 3)))
(defconstant +drmp3-uint64-max+ #xffffffffffffffff)

(deftype %ub8-array () '(simple-array (unsigned-byte 8) (*)))

(defmacro %drmp3-table (type &rest values)
  `(make-array ,(length values) :element-type ',type :initial-contents ',values))

;;;----------------------------------------------------------------------------------
;;; Tables
;;;----------------------------------------------------------------------------------

(alexandria:define-constant +drmp3-tabs+
  (%drmp3-table fixnum
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 785 785 785 785 784 784 784 784 513 513
    513 513 513 513 513 513 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 -255 1313 1298
    1282 785 785 785 785 784 784 784 784 769 769 769 769 256 256 256 256 256 256 256 256 256 256 256 256 256
    256 256 256 290 288 -255 1313 1298 1282 769 769 769 769 529 529 529 529 529 529 529 529 528 528 528 528
    528 528 528 528 512 512 512 512 512 512 512 512 290 288 -253 -318 -351 -367 785 785 785 785 784 784 784
    784 769 769 769 769 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 819 818 547 547 275
    275 275 275 561 560 515 546 289 274 288 258 -254 -287 1329 1299 1314 1312 1057 1057 1042 1042 1026 1026
    784 784 784 784 529 529 529 529 529 529 529 529 769 769 769 769 768 768 768 768 563 560 306 306 291 259
    -252 -413 -477 -542 1298 -575 1041 1041 784 784 784 784 769 769 769 769 256 256 256 256 256 256 256 256
    256 256 256 256 256 256 256 256 -383 -399 1107 1092 1106 1061 849 849 789 789 1104 1091 773 773 1076 1075
    341 340 325 309 834 804 577 577 532 532 516 516 832 818 803 816 561 561 531 531 515 546 289 289 288 258
    -252 -429 -493 -559 1057 1057 1042 1042 529 529 529 529 529 529 529 529 784 784 784 784 769 769 769 769
    512 512 512 512 512 512 512 512 -382 1077 -415 1106 1061 1104 849 849 789 789 1091 1076 1029 1075 834 834
    597 581 340 340 339 324 804 833 532 532 832 772 818 803 817 787 816 771 290 290 290 290 288 258 -253 -349
    -414 -447 -463 1329 1299 -479 1314 1312 1057 1057 1042 1042 1026 1026 785 785 785 785 784 784 784 784 769
    769 769 769 768 768 768 768 -319 851 821 -335 836 850 805 849 341 340 325 336 533 533 579 579 564 564 773
    832 578 548 563 516 321 276 306 291 304 259 -251 -572 -733 -830 -863 -879 1041 1041 784 784 784 784 769
    769 769 769 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 256 -511 -527 -543 1396 1351 1381
    1366 1395 1335 1380 -559 1334 1138 1138 1063 1063 1350 1392 1031 1031 1062 1062 1364 1363 1120 1120 1333
    1348 881 881 881 881 375 374 359 373 343 358 341 325 791 791 1123 1122 -703 1105 1045 -719 865 865 790
    790 774 774 1104 1029 338 293 323 308 -799 -815 833 788 772 818 803 816 322 292 307 320 561 531 515 546
    289 274 288 258 -251 -525 -605 -685 -765 -831 -846 1298 1057 1057 1312 1282 785 785 785 785 784 784 784
    784 769 769 769 769 512 512 512 512 512 512 512 512 1399 1398 1383 1367 1382 1396 1351 -511 1381 1366
    1139 1139 1079 1079 1124 1124 1364 1349 1363 1333 882 882 882 882 807 807 807 807 1094 1094 1136 1136 373
    341 535 535 881 775 867 822 774 -591 324 338 -671 849 550 550 866 864 609 609 293 336 534 534 789 835 773
    -751 834 804 308 307 833 788 832 772 562 562 547 547 305 275 560 515 290 290 -252 -397 -477 -557 -622
    -653 -719 -735 -750 1329 1299 1314 1057 1057 1042 1042 1312 1282 1024 1024 785 785 785 785 784 784 784
    784 769 769 769 769 -383 1127 1141 1111 1126 1140 1095 1110 869 869 883 883 1079 1109 882 882 375 374 807
    868 838 881 791 -463 867 822 368 263 852 837 836 -543 610 610 550 550 352 336 534 534 865 774 851 821 850
    805 593 533 579 564 773 832 578 578 548 548 577 577 307 276 306 291 516 560 259 259 -250 -2107 -2507
    -2764 -2909 -2974 -3007 -3023 1041 1041 1040 1040 769 769 769 769 256 256 256 256 256 256 256 256 256 256
    256 256 256 256 256 256 -767 -1052 -1213 -1277 -1358 -1405 -1469 -1535 -1550 -1582 -1614 -1647 -1662
    -1694 -1726 -1759 -1774 -1807 -1822 -1854 -1886 1565 -1919 -1935 -1951 -1967 1731 1730 1580 1717 -1983
    1729 1564 -1999 1548 -2015 -2031 1715 1595 -2047 1714 -2063 1610 -2079 1609 -2095 1323 1323 1457 1457
    1307 1307 1712 1547 1641 1700 1699 1594 1685 1625 1442 1442 1322 1322 -780 -973 -910 1279 1278 1277 1262
    1276 1261 1275 1215 1260 1229 -959 974 974 989 989 -943 735 478 478 495 463 506 414 -1039 1003 958 1017
    927 942 987 957 431 476 1272 1167 1228 -1183 1256 -1199 895 895 941 941 1242 1227 1212 1135 1014 1014 490
    489 503 487 910 1013 985 925 863 894 970 955 1012 847 -1343 831 755 755 984 909 428 366 754 559 -1391 752
    486 457 924 997 698 698 983 893 740 740 908 877 739 739 667 667 953 938 497 287 271 271 683 606 590 712
    726 574 302 302 738 736 481 286 526 725 605 711 636 724 696 651 589 681 666 710 364 467 573 695 466 466
    301 465 379 379 709 604 665 679 316 316 634 633 436 436 464 269 424 394 452 332 438 363 347 408 393 448
    331 422 362 407 392 421 346 406 391 376 375 359 1441 1306 -2367 1290 -2383 1337 -2399 -2415 1426 1321
    -2431 1411 1336 -2447 -2463 -2479 1169 1169 1049 1049 1424 1289 1412 1352 1319 -2495 1154 1154 1064 1064
    1153 1153 416 390 360 404 403 389 344 374 373 343 358 372 327 357 342 311 356 326 1395 1394 1137 1137
    1047 1047 1365 1392 1287 1379 1334 1364 1349 1378 1318 1363 792 792 792 792 1152 1152 1032 1032 1121 1121
    1046 1046 1120 1120 1030 1030 -2895 1106 1061 1104 849 849 789 789 1091 1076 1029 1090 1060 1075 833 833
    309 324 532 532 832 772 818 803 561 561 531 560 515 546 289 274 288 258 -250 -1179 -1579 -1836 -1996
    -2124 -2253 -2333 -2413 -2477 -2542 -2574 -2607 -2622 -2655 1314 1313 1298 1312 1282 785 785 785 785 1040
    1040 1025 1025 768 768 768 768 -766 -798 -830 -862 -895 -911 -927 -943 -959 -975 -991 -1007 -1023 -1039
    -1055 -1070 1724 1647 -1103 -1119 1631 1767 1662 1738 1708 1723 -1135 1780 1615 1779 1599 1677 1646 1778
    1583 -1151 1777 1567 1737 1692 1765 1722 1707 1630 1751 1661 1764 1614 1736 1676 1763 1750 1645 1598 1721
    1691 1762 1706 1582 1761 1566 -1167 1749 1629 767 766 751 765 494 494 735 764 719 749 734 763 447 447 748
    718 477 506 431 491 446 476 461 505 415 430 475 445 504 399 460 489 414 503 383 474 429 459 502 502 746
    752 488 398 501 473 413 472 486 271 480 270 -1439 -1455 1357 -1471 -1487 -1503 1341 1325 -1519 1489 1463
    1403 1309 -1535 1372 1448 1418 1476 1356 1462 1387 -1551 1475 1340 1447 1402 1386 -1567 1068 1068 1474
    1461 455 380 468 440 395 425 410 454 364 467 466 464 453 269 409 448 268 432 1371 1473 1432 1417 1308
    1460 1355 1446 1459 1431 1083 1083 1401 1416 1458 1445 1067 1067 1370 1457 1051 1051 1291 1430 1385 1444
    1354 1415 1400 1443 1082 1082 1173 1113 1186 1066 1185 1050 -1967 1158 1128 1172 1097 1171 1081 -1983
    1157 1112 416 266 375 400 1170 1142 1127 1065 793 793 1169 1033 1156 1096 1141 1111 1155 1080 1126 1140
    898 898 808 808 897 897 792 792 1095 1152 1032 1125 1110 1139 1079 1124 882 807 838 881 853 791 -2319 867
    368 263 822 852 837 866 806 865 -2399 851 352 262 534 534 821 836 594 594 549 549 593 593 533 533 848 773
    579 579 564 578 548 563 276 276 577 576 306 291 516 560 305 305 275 259 -251 -892 -2058 -2620 -2828 -2957
    -3023 -3039 1041 1041 1040 1040 769 769 769 769 256 256 256 256 256 256 256 256 256 256 256 256 256 256
    256 256 -511 -527 -543 -559 1530 -575 -591 1528 1527 1407 1526 1391 1023 1023 1023 1023 1525 1375 1268
    1268 1103 1103 1087 1087 1039 1039 1523 -604 815 815 815 815 510 495 509 479 508 463 507 447 431 505 415
    399 -734 -782 1262 -815 1259 1244 -831 1258 1228 -847 -863 1196 -879 1253 987 987 748 -767 493 493 462
    477 414 414 686 669 478 446 461 445 474 429 487 458 412 471 1266 1264 1009 1009 799 799 -1019 -1276 -1452
    -1581 -1677 -1757 -1821 -1886 -1933 -1997 1257 1257 1483 1468 1512 1422 1497 1406 1467 1496 1421 1510
    1134 1134 1225 1225 1466 1451 1374 1405 1252 1252 1358 1480 1164 1164 1251 1251 1238 1238 1389 1465 -1407
    1054 1101 -1423 1207 -1439 830 830 1248 1038 1237 1117 1223 1148 1236 1208 411 426 395 410 379 269 1193
    1222 1132 1235 1221 1116 976 976 1192 1162 1177 1220 1131 1191 963 963 -1647 961 780 -1663 558 558 994
    993 437 408 393 407 829 978 813 797 947 -1743 721 721 377 392 844 950 828 890 706 706 812 859 796 960 948
    843 934 874 571 571 -1919 690 555 689 421 346 539 539 944 779 918 873 932 842 903 888 570 570 931 917 674
    674 -2575 1562 -2591 1609 -2607 1654 1322 1322 1441 1441 1696 1546 1683 1593 1669 1624 1426 1426 1321
    1321 1639 1680 1425 1425 1305 1305 1545 1668 1608 1623 1667 1592 1638 1666 1320 1320 1652 1607 1409 1409
    1304 1304 1288 1288 1664 1637 1395 1395 1335 1335 1622 1636 1394 1394 1319 1319 1606 1621 1392 1392 1137
    1137 1137 1137 345 390 360 375 404 373 1047 -2751 -2767 -2783 1062 1121 1046 -2799 1077 -2815 1106 1061
    789 789 1105 1104 263 355 310 340 325 354 352 262 339 324 1091 1076 1029 1090 1060 1075 833 833 788 788
    1088 1028 818 818 803 803 561 561 531 531 816 771 546 546 289 274 288 258 -253 -317 -381 -446 -478 -509
    1279 1279 -811 -1179 -1451 -1756 -1900 -2028 -2189 -2253 -2333 -2414 -2445 -2511 -2526 1313 1298 -2559
    1041 1041 1040 1040 1025 1025 1024 1024 1022 1007 1021 991 1020 975 1019 959 687 687 1018 1017 671 671
    655 655 1016 1015 639 639 758 758 623 623 757 607 756 591 755 575 754 559 543 543 1009 783 -575 -621 -685
    -749 496 -590 750 749 734 748 974 989 1003 958 988 973 1002 942 987 957 972 1001 926 986 941 971 956 1000
    910 985 925 999 894 970 -1071 -1087 -1102 1390 -1135 1436 1509 1451 1374 -1151 1405 1358 1480 1420 -1167
    1507 1494 1389 1342 1465 1435 1450 1326 1505 1310 1493 1373 1479 1404 1492 1464 1419 428 443 472 397 736
    526 464 464 486 457 442 471 484 482 1357 1449 1434 1478 1388 1491 1341 1490 1325 1489 1463 1403 1309 1477
    1372 1448 1418 1433 1476 1356 1462 1387 -1439 1475 1340 1447 1402 1474 1324 1461 1371 1473 269 448 1432
    1417 1308 1460 -1711 1459 -1727 1441 1099 1099 1446 1386 1431 1401 -1743 1289 1083 1083 1160 1160 1458
    1445 1067 1067 1370 1457 1307 1430 1129 1129 1098 1098 268 432 267 416 266 400 -1887 1144 1187 1082 1173
    1113 1186 1066 1050 1158 1128 1143 1172 1097 1171 1081 420 391 1157 1112 1170 1142 1127 1065 1169 1049
    1156 1096 1141 1111 1155 1080 1126 1154 1064 1153 1140 1095 1048 -2159 1125 1110 1137 -2175 823 823 1139
    1138 807 807 384 264 368 263 868 838 853 791 867 822 852 837 866 806 865 790 -2319 851 821 836 352 262
    850 805 849 -2399 533 533 835 820 336 261 578 548 563 577 532 532 832 772 562 562 547 547 305 275 560 515
    290 290 288 258)
  :test #'equalp)

(alexandria:define-constant +drmp3-tab32+
  (%drmp3-table fixnum
    130 162 193 209 44 28 76 140 9 9 9 9 9 9 9 9 190 254 222 238 126 94 157 157 109 61 173 205)
  :test #'equalp)

(alexandria:define-constant +drmp3-tab33+
  (%drmp3-table fixnum
    252 236 220 204 188 172 156 140 124 108 92 76 60 44 28 12)
  :test #'equalp)

(alexandria:define-constant +drmp3-tabindex+
  (%drmp3-table fixnum
    0 32 64 98 0 132 180 218 292 364 426 538 648 746 0 1126 1460 1460 1460 1460 1460 1460 1460 1460 1842 1842
    1842 1842 1842 1842 1842 1842)
  :test #'equalp)

(alexandria:define-constant +drmp3-linbits+
  (%drmp3-table fixnum
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 3 4 6 8 10 13 4 5 6 7 8 9 11 13)
  :test #'equalp)

(alexandria:define-constant +drmp3-pow43+
  (%drmp3-table single-float
    0.0f0 -1.0f0 -2.519842f0 -4.326749f0 -6.349604f0 -8.549880f0 -10.902724f0 -13.390518f0 -16.000000f0
    -18.720754f0 -21.544347f0 -24.463781f0 -27.473142f0 -30.567351f0 -33.741992f0 -36.993181f0 0.0f0 1.0f0
    2.519842f0 4.326749f0 6.349604f0 8.549880f0 10.902724f0 13.390518f0 16.000000f0 18.720754f0 21.544347f0
    24.463781f0 27.473142f0 30.567351f0 33.741992f0 36.993181f0 40.317474f0 43.711787f0 47.173345f0
    50.699631f0 54.288352f0 57.937408f0 61.644865f0 65.408941f0 69.227979f0 73.100443f0 77.024898f0
    81.000000f0 85.024491f0 89.097188f0 93.216975f0 97.382800f0 101.593667f0 105.848633f0 110.146801f0
    114.487321f0 118.869381f0 123.292209f0 127.755065f0 132.257246f0 136.798076f0 141.376907f0 145.993119f0
    150.646117f0 155.335327f0 160.060199f0 164.820202f0 169.614826f0 174.443577f0 179.305980f0 184.201575f0
    189.129918f0 194.090580f0 199.083145f0 204.107210f0 209.162385f0 214.248292f0 219.364564f0 224.510845f0
    229.686789f0 234.892058f0 240.126328f0 245.389280f0 250.680604f0 256.000000f0 261.347174f0 266.721841f0
    272.123723f0 277.552547f0 283.008049f0 288.489971f0 293.998060f0 299.532071f0 305.091761f0 310.676898f0
    316.287249f0 321.922592f0 327.582707f0 333.267377f0 338.976394f0 344.709550f0 350.466646f0 356.247482f0
    362.051866f0 367.879608f0 373.730522f0 379.604427f0 385.501143f0 391.420496f0 397.362314f0 403.326427f0
    409.312672f0 415.320884f0 421.350905f0 427.402579f0 433.475750f0 439.570269f0 445.685987f0 451.822757f0
    457.980436f0 464.158883f0 470.357960f0 476.577530f0 482.817459f0 489.077615f0 495.357868f0 501.658090f0
    507.978156f0 514.317941f0 520.677324f0 527.056184f0 533.454404f0 539.871867f0 546.308458f0 552.764065f0
    559.238575f0 565.731879f0 572.243870f0 578.774440f0 585.323483f0 591.890898f0 598.476581f0 605.080431f0
    611.702349f0 618.342238f0 625.000000f0 631.675540f0 638.368763f0 645.079578f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-win+
  (%drmp3-table single-float
    -1.0f0 26.0f0 -31.0f0 208.0f0 218.0f0 401.0f0 -519.0f0 2063.0f0 2000.0f0 4788.0f0 -5517.0f0 7134.0f0
    5959.0f0 35640.0f0 -39336.0f0 74992.0f0 -1.0f0 24.0f0 -35.0f0 202.0f0 222.0f0 347.0f0 -581.0f0 2080.0f0
    1952.0f0 4425.0f0 -5879.0f0 7640.0f0 5288.0f0 33791.0f0 -41176.0f0 74856.0f0 -1.0f0 21.0f0 -38.0f0
    196.0f0 225.0f0 294.0f0 -645.0f0 2087.0f0 1893.0f0 4063.0f0 -6237.0f0 8092.0f0 4561.0f0 31947.0f0
    -43006.0f0 74630.0f0 -1.0f0 19.0f0 -41.0f0 190.0f0 227.0f0 244.0f0 -711.0f0 2085.0f0 1822.0f0 3705.0f0
    -6589.0f0 8492.0f0 3776.0f0 30112.0f0 -44821.0f0 74313.0f0 -1.0f0 17.0f0 -45.0f0 183.0f0 228.0f0 197.0f0
    -779.0f0 2075.0f0 1739.0f0 3351.0f0 -6935.0f0 8840.0f0 2935.0f0 28289.0f0 -46617.0f0 73908.0f0 -1.0f0
    16.0f0 -49.0f0 176.0f0 228.0f0 153.0f0 -848.0f0 2057.0f0 1644.0f0 3004.0f0 -7271.0f0 9139.0f0 2037.0f0
    26482.0f0 -48390.0f0 73415.0f0 -2.0f0 14.0f0 -53.0f0 169.0f0 227.0f0 111.0f0 -919.0f0 2032.0f0 1535.0f0
    2663.0f0 -7597.0f0 9389.0f0 1082.0f0 24694.0f0 -50137.0f0 72835.0f0 -2.0f0 13.0f0 -58.0f0 161.0f0 224.0f0
    72.0f0 -991.0f0 2001.0f0 1414.0f0 2330.0f0 -7910.0f0 9592.0f0 70.0f0 22929.0f0 -51853.0f0 72169.0f0
    -2.0f0 11.0f0 -63.0f0 154.0f0 221.0f0 36.0f0 -1064.0f0 1962.0f0 1280.0f0 2006.0f0 -8209.0f0 9750.0f0
    -998.0f0 21189.0f0 -53534.0f0 71420.0f0 -2.0f0 10.0f0 -68.0f0 147.0f0 215.0f0 2.0f0 -1137.0f0 1919.0f0
    1131.0f0 1692.0f0 -8491.0f0 9863.0f0 -2122.0f0 19478.0f0 -55178.0f0 70590.0f0 -3.0f0 9.0f0 -73.0f0
    139.0f0 208.0f0 -29.0f0 -1210.0f0 1870.0f0 970.0f0 1388.0f0 -8755.0f0 9935.0f0 -3300.0f0 17799.0f0
    -56778.0f0 69679.0f0 -3.0f0 8.0f0 -79.0f0 132.0f0 200.0f0 -57.0f0 -1283.0f0 1817.0f0 794.0f0 1095.0f0
    -8998.0f0 9966.0f0 -4533.0f0 16155.0f0 -58333.0f0 68692.0f0 -4.0f0 7.0f0 -85.0f0 125.0f0 189.0f0 -83.0f0
    -1356.0f0 1759.0f0 605.0f0 814.0f0 -9219.0f0 9959.0f0 -5818.0f0 14548.0f0 -59838.0f0 67629.0f0 -4.0f0
    7.0f0 -91.0f0 117.0f0 177.0f0 -106.0f0 -1428.0f0 1698.0f0 402.0f0 545.0f0 -9416.0f0 9916.0f0 -7154.0f0
    12980.0f0 -61289.0f0 66494.0f0 -5.0f0 6.0f0 -97.0f0 111.0f0 163.0f0 -127.0f0 -1498.0f0 1634.0f0 185.0f0
    288.0f0 -9585.0f0 9838.0f0 -8540.0f0 11455.0f0 -62684.0f0 65290.0f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-scf-long+
  (%drmp3-table fixnum
    6 6 6 6 6 6 8 10 12 14 16 20 24 28 32 38 46 52 60 68 58 54 0 12 12 12 12 12 12 16 20 24 28 32 40 48 56 64
    76 90 2 2 2 2 2 0 6 6 6 6 6 6 8 10 12 14 16 20 24 28 32 38 46 52 60 68 58 54 0 6 6 6 6 6 6 8 10 12 14 16
    18 22 26 32 38 46 54 62 70 76 36 0 6 6 6 6 6 6 8 10 12 14 16 20 24 28 32 38 46 52 60 68 58 54 0 4 4 4 4 4
    4 6 6 8 8 10 12 16 20 24 28 34 42 50 54 76 158 0 4 4 4 4 4 4 6 6 6 8 10 12 16 18 22 28 34 40 46 54 54 192
    0 4 4 4 4 4 4 6 6 8 10 12 16 20 24 30 38 46 56 68 84 102 26 0)
  :test #'equalp)

(alexandria:define-constant +drmp3-scf-short+
  (%drmp3-table fixnum
    4 4 4 4 4 4 4 4 4 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 24 24 24 30 30 30 40 40 40 18 18 18 0 8
    8 8 8 8 8 8 8 8 12 12 12 16 16 16 20 20 20 24 24 24 28 28 28 36 36 36 2 2 2 2 2 2 2 2 2 26 26 26 0 4 4 4
    4 4 4 4 4 4 6 6 6 6 6 6 8 8 8 10 10 10 14 14 14 18 18 18 26 26 26 32 32 32 42 42 42 18 18 18 0 4 4 4 4 4
    4 4 4 4 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 24 24 24 32 32 32 44 44 44 12 12 12 0 4 4 4 4 4 4
    4 4 4 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 24 24 24 30 30 30 40 40 40 18 18 18 0 4 4 4 4 4 4 4
    4 4 4 4 4 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 22 22 22 30 30 30 56 56 56 0 4 4 4 4 4 4 4 4 4
    4 4 4 6 6 6 6 6 6 10 10 10 12 12 12 14 14 14 16 16 16 20 20 20 26 26 26 66 66 66 0 4 4 4 4 4 4 4 4 4 4 4
    4 6 6 6 8 8 8 12 12 12 16 16 16 20 20 20 26 26 26 34 34 34 42 42 42 12 12 12 0)
  :test #'equalp)

(alexandria:define-constant +drmp3-scf-mixed+
  (%drmp3-table fixnum
    6 6 6 6 6 6 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 24 24 24 30 30 30 40 40 40 18 18 18 0 0 0 0
    12 12 12 4 4 4 8 8 8 12 12 12 16 16 16 20 20 20 24 24 24 28 28 28 36 36 36 2 2 2 2 2 2 2 2 2 26 26 26 0 6
    6 6 6 6 6 6 6 6 6 6 6 8 8 8 10 10 10 14 14 14 18 18 18 26 26 26 32 32 32 42 42 42 18 18 18 0 0 0 0 6 6 6
    6 6 6 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 24 24 24 32 32 32 44 44 44 12 12 12 0 0 0 0 6 6 6 6
    6 6 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 24 24 24 30 30 30 40 40 40 18 18 18 0 0 0 0 4 4 4 4 4
    4 6 6 4 4 4 6 6 6 8 8 8 10 10 10 12 12 12 14 14 14 18 18 18 22 22 22 30 30 30 56 56 56 0 0 4 4 4 4 4 4 6
    6 4 4 4 6 6 6 6 6 6 10 10 10 12 12 12 14 14 14 16 16 16 20 20 20 26 26 26 66 66 66 0 0 4 4 4 4 4 4 6 6 4
    4 4 6 6 6 8 8 8 12 12 12 16 16 16 20 20 20 26 26 26 34 34 34 42 42 42 12 12 12 0 0)
  :test #'equalp)

(alexandria:define-constant +drmp3-sec+
  (%drmp3-table single-float
    10.19000816f0 0.50060302f0 0.50241929f0 3.40760851f0 0.50547093f0 0.52249861f0 2.05778098f0 0.51544732f0
    0.56694406f0 1.48416460f0 0.53104258f0 0.64682180f0 1.16943991f0 0.55310392f0 0.78815460f0 0.97256821f0
    0.58293498f0 1.06067765f0 0.83934963f0 0.62250412f0 1.72244716f0 0.74453628f0 0.67480832f0 5.10114861f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-mdct-window+
  (%drmp3-table single-float
    0.99904822f0 0.99144486f0 0.97629601f0 0.95371695f0 0.92387953f0 0.88701083f0 0.84339145f0 0.79335334f0
    0.73727734f0 0.04361938f0 0.13052619f0 0.21643961f0 0.30070580f0 0.38268343f0 0.46174861f0 0.53729961f0
    0.60876143f0 0.67559021f0 1.0f0 1.0f0 1.0f0 1.0f0 1.0f0 1.0f0 0.99144486f0 0.92387953f0 0.79335334f0
    0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 0.13052619f0 0.38268343f0 0.60876143f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-twid9+
  (%drmp3-table single-float
    0.73727734f0 0.79335334f0 0.84339145f0 0.88701083f0 0.92387953f0 0.95371695f0 0.97629601f0 0.99144486f0
    0.99904822f0 0.67559021f0 0.60876143f0 0.53729961f0 0.46174861f0 0.38268343f0 0.30070580f0 0.21643961f0
    0.13052619f0 0.04361938f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-twid3+
  (%drmp3-table single-float
    0.79335334f0 0.92387953f0 0.99144486f0 0.60876143f0 0.38268343f0 0.13052619f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-aa+
  (%drmp3-table single-float
    0.85749293f0 0.88174200f0 0.94962865f0 0.98331459f0 0.99551782f0 0.99916056f0 0.99989920f0 0.99999316f0
    0.51449576f0 0.47173197f0 0.31337745f0 0.18191320f0 0.09457419f0 0.04096558f0 0.01419856f0 0.00369997f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-pan+
  (%drmp3-table single-float
    0.0f0 1.0f0 0.21132487f0 0.78867513f0 0.36602540f0 0.63397460f0 0.5f0 0.5f0 0.63397460f0 0.36602540f0
    0.78867513f0 0.21132487f0 1.0f0 0.0f0)
  :test #'equalp)

(alexandria:define-constant +drmp3-bitalloc-code-tab+
  (%drmp3-table fixnum
    0 17 3 4 5 6 7 8 9 10 11 12 13 14 15 16 0 17 18 3 19 4 5 6 7 8 9 10 11 12 13 16 0 17 18 3 19 4 5 16 0 17
    18 16 0 17 18 19 4 5 6 7 8 9 10 11 12 13 14 15 0 17 18 3 19 4 5 6 7 8 9 10 11 12 13 14 0 2 3 4 5 6 7 8 9
    10 11 12 13 14 15 16)
  :test #'equalp)

(alexandria:define-constant +drmp3-halfrate+
  (%drmp3-table fixnum
    0 4 8 12 16 20 24 28 32 40 48 56 64 72 80 0 4 8 12 16 20 24 28 32 40 48 56 64 72 80 0 16 24 28 32 40 48
    56 64 72 80 88 96 112 128 0 16 20 24 28 32 40 48 56 64 80 96 112 128 160 0 16 24 28 32 40 48 56 64 80 96
    112 128 160 192 0 16 32 48 64 80 96 112 128 144 160 176 192 208 224)
  :test #'equalp)

(alexandria:define-constant +drmp3-expfrac+
  (%drmp3-table single-float
    9.31322575f-10 7.83145814f-10 6.58544508f-10 5.53767716f-10)
  :test #'equalp)

(alexandria:define-constant +drmp3-scf-partitions+
  (%drmp3-table fixnum
    6 5 5 5 6 5 5 5 6 5 7 3 11 10 0 0 7 7 7 0 6 6 6 3 8 8 5 0 8 9 6 12 6 9 9 9 6 9 12 6 15 18 0 0 6 15 12 0 6
    12 9 6 6 18 9 0 9 9 6 12 9 9 9 9 9 9 12 6 18 18 0 0 12 12 12 0 12 9 9 6 15 12 9 0)
  :test #'equalp)

(alexandria:define-constant +drmp3-mod+
  (%drmp3-table fixnum
    5 5 4 4 5 5 4 1 4 3 1 1 5 6 6 1 4 4 4 1 4 3 1 1)
  :test #'equalp)

(alexandria:define-constant +drmp3-preamp+
  (%drmp3-table fixnum
    1 1 1 1 2 2 3 3 3 2)
  :test #'equalp)

(alexandria:define-constant +drmp3-scfc-decode+
  (%drmp3-table fixnum
    0 1 2 3 12 5 6 7 9 10 11 13 14 15 18 19)
  :test #'equalp)

;;;----------------------------------------------------------------------------------
;;; Types and Structures Definition
;;;----------------------------------------------------------------------------------

(defstruct (drmp3-bs (:constructor %make-drmp3-bs (buf base pos limit)))
  (buf nil :type %ub8-array)
  (base 0 :type fixnum)                         ; Offset of the first byte in BUF
  (pos 0 :type fixnum)                          ; Position in bits
  (limit 0 :type fixnum))

(defstruct (drmp3-gr-info (:conc-name gr-))
  (sfbtab nil) (sfbtab-off 0)                   ; Scalefactor band table row
  (part-23-length 0) (big-values 0) (scalefac-compress 0)
  (global-gain 0) (block-type 0) (mixed-block-flag 0) (n-long-sfb 0) (n-short-sfb 0)
  (table-select (make-array 3 :initial-element 0))
  (region-count (make-array 3 :initial-element 0))
  (subblock-gain (make-array 3 :initial-element 0))
  (preflag 0) (scalefac-scale 0) (count1-table 0) (scfsi 0))

(defstruct (drmp3dec (:constructor %make-drmp3dec))
  (mdct-overlap (make-array (* 2 9 32) :element-type 'single-float :initial-element 0f0) :type %f32-array)
  (qmf-state (make-array (* 15 2 32) :element-type 'single-float :initial-element 0f0) :type %f32-array)
  (reserv 0 :type fixnum)
  (free-format-bytes 0 :type fixnum)
  (header (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0) :type %ub8-array)
  (reserv-buf (make-array 511 :element-type '(unsigned-byte 8) :initial-element 0) :type %ub8-array)
  ;; Scratch
  (bs nil)
  (maindata (make-array (+ +drmp3-max-bitreservoir-bytes+ +drmp3-max-l3-frame-payload-bytes+ 8)
                        :element-type '(unsigned-byte 8) :initial-element 0)
   :type %ub8-array)
  (gr-info (let ((v (make-array 4))) (dotimes (i 4 v) (setf (svref v i) (make-drmp3-gr-info)))))
  (grbuf (make-array (* 2 576) :element-type 'single-float :initial-element 0f0) :type %f32-array)
  (scf (make-array 40 :element-type 'single-float :initial-element 0f0) :type %f32-array)
  (syn (make-array (* (+ 18 15) 2 32) :element-type 'single-float :initial-element 0f0) :type %f32-array)
  (ist-pos (make-array (* 2 39) :element-type '(unsigned-byte 8) :initial-element 0) :type %ub8-array))

(defun drmp3dec-zero (dec)
  "DRMP3_ZERO_MEMORY(dec, sizeof(drmp3dec))"
  (fill (drmp3dec-mdct-overlap dec) 0f0)
  (fill (drmp3dec-qmf-state dec) 0f0)
  (setf (drmp3dec-reserv dec) 0
        (drmp3dec-free-format-bytes dec) 0)
  (fill (drmp3dec-header dec) 0)
  (fill (drmp3dec-reserv-buf dec) 0)
  (fill (drmp3dec-maindata dec) 0)
  (dotimes (i 4) (setf (svref (drmp3dec-gr-info dec) i) (make-drmp3-gr-info)))
  (fill (drmp3dec-grbuf dec) 0f0)
  (fill (drmp3dec-scf dec) 0f0)
  (fill (drmp3dec-syn dec) 0f0)
  (fill (drmp3dec-ist-pos dec) 0))

(defun drmp3dec-init (dec)
  (setf (aref (drmp3dec-header dec) 0) 0))

;;;----------------------------------------------------------------------------------
;;; Bitstream and header
;;;----------------------------------------------------------------------------------

(defun %drmp3-bs-get-bits (bs n)
  (let* ((s (logand (drmp3-bs-pos bs) 7))
         (shl (+ n s))
         (buf (drmp3-bs-buf bs))
         (p (+ (drmp3-bs-base bs) (ash (drmp3-bs-pos bs) -3)))
         (cache 0) (next 0))
    (when (> (incf (drmp3-bs-pos bs) n) (drmp3-bs-limit bs))
      (return-from %drmp3-bs-get-bits 0))
    (setf next (logand (aref buf p) (ash 255 (- s))))
    (incf p)
    (loop while (> (decf shl 8) 0)
          do (setf cache (logior cache (ash next shl))
                   next (aref buf p))
             (incf p))
    (logand (logior cache (ash next shl)) #xffffffff)))

(declaim (inline %h1 %h2 %h3))
(defun %h1 (h o) (aref h (+ o 1)))
(defun %h2 (h o) (aref h (+ o 2)))
(defun %h3 (h o) (aref h (+ o 3)))
(defun %drmp3-hdr-is-mono (h o) (= (logand (%h3 h o) #xc0) #xc0))
(defun %drmp3-hdr-is-ms-stereo (h o) (= (logand (%h3 h o) #xe0) #x60))
(defun %drmp3-hdr-is-free-format (h o) (= (logand (%h2 h o) #xf0) 0))
(defun %drmp3-hdr-is-crc (h o) (not (logtest (%h1 h o) 1)))
(defun %drmp3-hdr-test-padding (h o) (logtest (%h2 h o) 2))
(defun %drmp3-hdr-test-mpeg1 (h o) (logtest (%h1 h o) 8))
(defun %drmp3-hdr-test-not-mpeg25 (h o) (logtest (%h1 h o) #x10))
(defun %drmp3-hdr-test-i-stereo (h o) (logtest (%h3 h o) #x10))
(defun %drmp3-hdr-test-ms-stereo (h o) (logtest (%h3 h o) #x20))
(defun %drmp3-hdr-get-stereo-mode (h o) (logand (ash (%h3 h o) -6) 3))
(defun %drmp3-hdr-get-stereo-mode-ext (h o) (logand (ash (%h3 h o) -4) 3))
(defun %drmp3-hdr-get-layer (h o) (logand (ash (%h1 h o) -1) 3))
(defun %drmp3-hdr-get-bitrate (h o) (ash (%h2 h o) -4))
(defun %drmp3-hdr-get-sample-rate (h o) (logand (ash (%h2 h o) -2) 3))
(defun %drmp3-hdr-get-my-sample-rate (h o)
  (+ (%drmp3-hdr-get-sample-rate h o) (* (+ (logand (ash (%h1 h o) -3) 1) (logand (ash (%h1 h o) -4) 1)) 3)))
(defun %drmp3-hdr-is-frame-576 (h o) (= (logand (%h1 h o) 14) 2))
(defun %drmp3-hdr-is-layer-1 (h o) (= (logand (%h1 h o) 6) 6))

(defun %drmp3-hdr-valid (h o)
  (and (= (aref h o) #xff)
       (or (= (logand (%h1 h o) #xf0) #xf0) (= (logand (%h1 h o) #xfe) #xe2))
       (/= (%drmp3-hdr-get-layer h o) 0)
       (/= (%drmp3-hdr-get-bitrate h o) 15)
       (/= (%drmp3-hdr-get-sample-rate h o) 3)))

(defun %drmp3-hdr-compare (h1 o1 h2 o2)
  (and (%drmp3-hdr-valid h2 o2)
       (= (logand (logxor (%h1 h1 o1) (%h1 h2 o2)) #xfe) 0)
       (= (logand (logxor (%h2 h1 o1) (%h2 h2 o2)) #x0c) 0)
       (eq (%drmp3-hdr-is-free-format h1 o1) (%drmp3-hdr-is-free-format h2 o2))))

(defun %drmp3-hdr-bitrate-kbps (h o)
  (* 2 (aref +drmp3-halfrate+ (+ (* (if (%drmp3-hdr-test-mpeg1 h o) 1 0) 45)
                                 (* (- (%drmp3-hdr-get-layer h o) 1) 15)
                                 (%drmp3-hdr-get-bitrate h o)))))

(defun %drmp3-hdr-sample-rate-hz (h o)
  (ash (ash (aref #(44100 48000 32000) (%drmp3-hdr-get-sample-rate h o))
            (- (if (%drmp3-hdr-test-mpeg1 h o) 0 1)))
       (- (if (%drmp3-hdr-test-not-mpeg25 h o) 0 1))))

(defun %drmp3-hdr-frame-samples (h o)
  (if (%drmp3-hdr-is-layer-1 h o) 384 (ash 1152 (- (if (%drmp3-hdr-is-frame-576 h o) 1 0)))))

(defun %drmp3-hdr-frame-bytes (h o free-format-size)
  (let ((frame-bytes (floor (* (%drmp3-hdr-frame-samples h o) (%drmp3-hdr-bitrate-kbps h o) 125)
                            (%drmp3-hdr-sample-rate-hz h o))))
    (when (%drmp3-hdr-is-layer-1 h o)
      (setf frame-bytes (logand frame-bytes (lognot 3))))   ; Slot align
    (if (/= frame-bytes 0) frame-bytes free-format-size)))

(defun %drmp3-hdr-padding (h o)
  (if (%drmp3-hdr-test-padding h o) (if (%drmp3-hdr-is-layer-1 h o) 4 1) 0))

;;;----------------------------------------------------------------------------------
;;; Layer I/II
;;;----------------------------------------------------------------------------------

(defstruct (drmp3-l12-scale-info (:conc-name sci-))
  (scf (make-array (* 3 64) :element-type 'single-float :initial-element 0f0) :type %f32-array)
  (total-bands 0) (stereo-bands 0)
  (bitalloc (make-array 64 :initial-element 0))
  (scfcod (make-array 64 :initial-element 0)))

(defvar *drmp3-deq-l12*
  (let ((v (make-array (* 18 3) :element-type 'single-float))
        (i 0))
    (dolist (x '(3 7 15 31 63 127 255 511 1023 2047 4095 8191 16383 32767 65535 3 5 9) v)
      (dolist (c '(9.53674316f-07 7.56931807f-07 6.00777173f-07))
        (setf (aref v i) (/ c (float x 1f0)))
        (incf i)))))

;; Returns the subband alloc table as a list of (tab_offset code_tab_width band_count)
(defun %drmp3-l12-subband-alloc-table (hdr o sci)
  (let* ((mode (%drmp3-hdr-get-stereo-mode hdr o))
         (stereo-bands (cond ((= mode +drmp3-mode-mono+) 0)
                             ((= mode +drmp3-mode-joint-stereo+) (+ (ash (%drmp3-hdr-get-stereo-mode-ext hdr o) 2) 4))
                             (t 32)))
         (alloc nil) (nbands 0))
    (cond ((%drmp3-hdr-is-layer-1 hdr o)
           (setf alloc '((76 4 32)) nbands 32))
          ((not (%drmp3-hdr-test-mpeg1 hdr o))
           (setf alloc '((60 4 4) (44 3 7) (44 2 19)) nbands 30))
          (t
           (let ((sample-rate-idx (%drmp3-hdr-get-sample-rate hdr o))
                 (kbps (ash (%drmp3-hdr-bitrate-kbps hdr o) (- (if (/= mode +drmp3-mode-mono+) 1 0)))))
             (when (= kbps 0) (setf kbps 192))   ; Free-format
             (setf alloc '((0 4 3) (16 4 8) (32 3 12) (40 2 7)) nbands 27)
             (cond ((< kbps 56)
                    (setf alloc '((44 4 2) (44 3 10))
                          nbands (if (= sample-rate-idx 2) 12 8)))
                   ((and (>= kbps 96) (/= sample-rate-idx 1))
                    (setf nbands 30))))))
    (setf (sci-total-bands sci) nbands
          (sci-stereo-bands sci) (min stereo-bands nbands))
    alloc))

(defun %drmp3-l12-read-scalefactors (bs bitalloc scfcod bands scf)
  (let ((si 0))
    (dotimes (i bands)
      (let* ((s 0f0)
             (ba (aref bitalloc i))
             (mask (if (/= ba 0) (+ 4 (logand (ash 19 (- (aref scfcod i))) 3)) 0)))
        (loop for m = 4 then (ash m -1)
              while (/= m 0)
              do (when (logtest mask m)
                   (let ((b (%drmp3-bs-get-bits bs 6)))
                     (setf s (* (aref *drmp3-deq-l12* (+ (- (* ba 3) 6) (mod b 3)))
                                (float (ash (ash 1 21) (- (floor b 3))) 1f0)))))
                 (setf (aref scf si) s)
                 (incf si))))))

(defun %drmp3-l12-read-scale-info (hdr o bs sci)
  (let ((subband-alloc (%drmp3-l12-subband-alloc-table hdr o sci))
        (k 0) (ba-bits 0) (ba-code-tab 0))
    (dotimes (i (sci-total-bands sci))
      (when (= i k)
        (destructuring-bind (tab-offset code-tab-width band-count) (pop subband-alloc)
          (incf k band-count)
          (setf ba-bits code-tab-width
                ba-code-tab tab-offset)))
      (let ((ba (aref +drmp3-bitalloc-code-tab+ (+ ba-code-tab (%drmp3-bs-get-bits bs ba-bits)))))
        (setf (aref (sci-bitalloc sci) (* 2 i)) ba)
        (when (< i (sci-stereo-bands sci))
          (setf ba (aref +drmp3-bitalloc-code-tab+ (+ ba-code-tab (%drmp3-bs-get-bits bs ba-bits)))))
        (setf (aref (sci-bitalloc sci) (+ (* 2 i) 1)) (if (/= (sci-stereo-bands sci) 0) ba 0))))
    (dotimes (i (* 2 (sci-total-bands sci)))
      (setf (aref (sci-scfcod sci) i)
            (if (/= (aref (sci-bitalloc sci) i) 0)
                (if (%drmp3-hdr-is-layer-1 hdr o) 2 (%drmp3-bs-get-bits bs 2))
                6)))
    (%drmp3-l12-read-scalefactors bs (sci-bitalloc sci) (sci-scfcod sci) (* (sci-total-bands sci) 2) (sci-scf sci))
    (loop for i from (sci-stereo-bands sci) below (sci-total-bands sci)
          do (setf (aref (sci-bitalloc sci) (+ (* 2 i) 1)) 0))))

(defun %drmp3-l12-dequantize-granule (grbuf start bs sci group-size)
  (let ((choff 576))
    (dotimes (j 4)
      (let ((dst (+ start (* group-size j))))
        (dotimes (i (* 2 (sci-total-bands sci)))
          (let ((ba (aref (sci-bitalloc sci) i)))
            (when (/= ba 0)
              (if (< ba 17)
                  (let ((half (- (ash 1 (- ba 1)) 1)))
                    (dotimes (k group-size)
                      (setf (aref grbuf (+ dst k)) (float (- (%drmp3-bs-get-bits bs ba) half) 1f0))))
                  (let* ((mod (+ (ash 2 (- ba 17)) 1))   ; 3, 5, 9
                         (code (%drmp3-bs-get-bits bs (- (+ mod 2) (ash mod -3)))))   ; 5, 7, 10
                    (dotimes (k group-size)
                      (setf (aref grbuf (+ dst k)) (float (- (mod code mod) (floor mod 2)) 1f0))
                      (setf code (floor code mod)))))))
          (incf dst choff)
          (setf choff (- 18 choff)))))
    (* group-size 4)))

(defun %drmp3-l12-apply-scf-384 (sci scf-start dst)
  (let ((scf (sci-scf sci))
        (stereo (sci-stereo-bands sci)))
    (replace dst dst :start1 (+ 576 (* stereo 18)) :start2 (* stereo 18)
                     :end2 (+ (* stereo 18) (* (- (sci-total-bands sci) stereo) 18)))
    (dotimes (i (sci-total-bands sci))
      (let ((d (* i 18)) (s (+ scf-start (* i 6))))
        (dotimes (k 12)
          (setf (aref dst (+ d k)) (* (aref dst (+ d k)) (aref scf s))
                (aref dst (+ d k 576)) (* (aref dst (+ d k 576)) (aref scf (+ s 3)))))))))

;;;----------------------------------------------------------------------------------
;;; Layer III
;;;----------------------------------------------------------------------------------

(defun %drmp3-l3-read-side-info (bs gr-infos hdr o)
  (let* ((tables 0) (scfsi 0) (main-data-begin 0) (part-23-sum 0)
         (gr-count (if (%drmp3-hdr-is-mono hdr o) 1 2))
         (sr-idx (let ((s (%drmp3-hdr-get-my-sample-rate hdr o))) (- s (if (/= s 0) 1 0))))
         (gi 0))
    (if (%drmp3-hdr-test-mpeg1 hdr o)
        (setf gr-count (* gr-count 2)
              main-data-begin (%drmp3-bs-get-bits bs 9)
              scfsi (%drmp3-bs-get-bits bs (+ 7 gr-count)))
        (setf main-data-begin (ash (%drmp3-bs-get-bits bs (+ 8 gr-count)) (- gr-count))))
    (loop
      (let ((gr (svref gr-infos gi)))
        (when (%drmp3-hdr-is-mono hdr o)
          (setf scfsi (ash scfsi 4)))
        (setf (gr-part-23-length gr) (%drmp3-bs-get-bits bs 12))
        (incf part-23-sum (gr-part-23-length gr))
        (setf (gr-big-values gr) (%drmp3-bs-get-bits bs 9))
        (when (> (gr-big-values gr) 288)
          (return-from %drmp3-l3-read-side-info -1))
        (setf (gr-global-gain gr) (%drmp3-bs-get-bits bs 8)
              (gr-scalefac-compress gr) (%drmp3-bs-get-bits bs (if (%drmp3-hdr-test-mpeg1 hdr o) 4 9))
              (gr-sfbtab gr) +drmp3-scf-long+
              (gr-sfbtab-off gr) (* sr-idx 23)
              (gr-n-long-sfb gr) 22
              (gr-n-short-sfb gr) 0)
        (if (/= (%drmp3-bs-get-bits bs 1) 0)
            (progn
              (setf (gr-block-type gr) (%drmp3-bs-get-bits bs 2))
              (when (= (gr-block-type gr) 0)
                (return-from %drmp3-l3-read-side-info -1))
              (setf (gr-mixed-block-flag gr) (%drmp3-bs-get-bits bs 1)
                    (aref (gr-region-count gr) 0) 7
                    (aref (gr-region-count gr) 1) 255)
              (when (= (gr-block-type gr) +drmp3-short-block-type+)
                (setf scfsi (logand scfsi #x0f0f))
                (if (= (gr-mixed-block-flag gr) 0)
                    (setf (aref (gr-region-count gr) 0) 8
                          (gr-sfbtab gr) +drmp3-scf-short+
                          (gr-sfbtab-off gr) (* sr-idx 40)
                          (gr-n-long-sfb gr) 0
                          (gr-n-short-sfb gr) 39)
                    (setf (gr-sfbtab gr) +drmp3-scf-mixed+
                          (gr-sfbtab-off gr) (* sr-idx 40)
                          (gr-n-long-sfb gr) (if (%drmp3-hdr-test-mpeg1 hdr o) 8 6)
                          (gr-n-short-sfb gr) 30)))
              (setf tables (ash (%drmp3-bs-get-bits bs 10) 5))
              (dotimes (i 3)
                (setf (aref (gr-subblock-gain gr) i) (%drmp3-bs-get-bits bs 3))))
            (progn
              (setf (gr-block-type gr) 0
                    (gr-mixed-block-flag gr) 0
                    tables (%drmp3-bs-get-bits bs 15)
                    (aref (gr-region-count gr) 0) (%drmp3-bs-get-bits bs 4)
                    (aref (gr-region-count gr) 1) (%drmp3-bs-get-bits bs 3)
                    (aref (gr-region-count gr) 2) 255)))
        (setf (aref (gr-table-select gr) 0) (logand (ash tables -10) #xff)
              (aref (gr-table-select gr) 1) (logand (ash tables -5) 31)
              (aref (gr-table-select gr) 2) (logand tables 31)
              (gr-preflag gr) (if (%drmp3-hdr-test-mpeg1 hdr o)
                                  (%drmp3-bs-get-bits bs 1)
                                  (if (>= (gr-scalefac-compress gr) 500) 1 0))
              (gr-scalefac-scale gr) (%drmp3-bs-get-bits bs 1)
              (gr-count1-table gr) (%drmp3-bs-get-bits bs 1)
              (gr-scfsi gr) (logand (ash scfsi -12) 15))
        (setf scfsi (ash scfsi 4))
        (incf gi)
        (when (= (decf gr-count) 0) (return))))
    (if (> (+ part-23-sum (drmp3-bs-pos bs)) (+ (drmp3-bs-limit bs) (* main-data-begin 8)))
        -1
        main-data-begin)))

(defun %drmp3-l3-read-scalefactors (scf ist-pos ist-off scf-size scf-count scf-count-off bitbuf scfsi)
  (let ((si 0) (ii ist-off))
    (loop for i from 0
          while (and (< i 4) (/= (aref scf-count (+ scf-count-off i)) 0))
          do (let ((cnt (aref scf-count (+ scf-count-off i))))
               (if (logtest scfsi 8)
                   (replace scf ist-pos :start1 si :start2 ii :end2 (+ ii cnt))
                   (let ((bits (aref scf-size i)))
                     (if (= bits 0)
                         (progn (fill scf 0 :start si :end (+ si cnt))
                                (fill ist-pos 0 :start ii :end (+ ii cnt)))
                         (let ((max-scf (if (< scfsi 0) (- (ash 1 bits) 1) -1)))
                           (dotimes (k cnt)
                             (let ((s (%drmp3-bs-get-bits bitbuf bits)))
                               (setf (aref ist-pos (+ ii k)) (logand (if (= s max-scf) -1 s) #xff)
                                     (aref scf (+ si k)) (logand s #xff))))))))
               (incf ii cnt)
               (incf si cnt)
               (setf scfsi (* scfsi 2))))
    (setf (aref scf si) 0 (aref scf (+ si 1)) 0 (aref scf (+ si 2)) 0)))

(defun %drmp3-l3-ldexp-q2 (y exp-q2)
  (declare (type single-float y))
  (loop
    (let ((e (min (* 30 4) exp-q2)))
      (setf y (* y (* (aref +drmp3-expfrac+ (logand e 3)) (float (ash (ash 1 30) (- (ash e -2))) 1f0))))
      (unless (> (decf exp-q2 e) 0) (return))))
  y)

(defun %drmp3-l3-decode-scalefactors (hdr o ist-pos ist-off bs gr scf ch)
  (let* ((scf-partition-off (* 28 (+ (if (/= (gr-n-short-sfb gr) 0) 1 0) (if (= (gr-n-long-sfb gr) 0) 1 0))))
         (scf-size (make-array 4 :initial-element 0))
         (iscf (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0))
         (scf-shift (+ (gr-scalefac-scale gr) 1))
         (scfsi (gr-scfsi gr)))
    (if (%drmp3-hdr-test-mpeg1 hdr o)
        (let ((part (aref +drmp3-scfc-decode+ (gr-scalefac-compress gr))))
          (setf (aref scf-size 1) (ash part -2) (aref scf-size 0) (ash part -2)
                (aref scf-size 3) (logand part 3) (aref scf-size 2) (logand part 3)))
        (let* ((ist (if (and (%drmp3-hdr-test-i-stereo hdr o) (/= ch 0)) 1 0))
               (sfc (ash (gr-scalefac-compress gr) (- ist)))
               (k (* ist 3 4))
               (modprod 0))
          (loop while (>= sfc 0)
                do (setf modprod 1)
                   (loop for i from 3 downto 0
                         do (setf (aref scf-size i) (logand (mod (floor sfc modprod) (aref +drmp3-mod+ (+ k i))) #xff))
                            (setf modprod (* modprod (aref +drmp3-mod+ (+ k i)))))
                   (decf sfc modprod)
                   (incf k 4))
          (incf scf-partition-off k)
          (setf scfsi -16)))
    (%drmp3-l3-read-scalefactors iscf ist-pos ist-off scf-size +drmp3-scf-partitions+ scf-partition-off bs scfsi)
    (if (/= (gr-n-short-sfb gr) 0)
        (let ((sh (- 3 scf-shift))
              (nl (gr-n-long-sfb gr)))
          (loop for i from 0 below (gr-n-short-sfb gr) by 3
                do (dotimes (j 3)
                     (setf (aref iscf (+ nl i j))
                           (logand (+ (aref iscf (+ nl i j)) (ash (aref (gr-subblock-gain gr) j) sh)) #xff)))))
        (when (/= (gr-preflag gr) 0)
          (dotimes (i 10)
            (setf (aref iscf (+ 11 i)) (logand (+ (aref iscf (+ 11 i)) (aref +drmp3-preamp+ i)) #xff)))))
    (let* ((gain-exp (- (+ (gr-global-gain gr) (* +drmp3-bits-dequantizer-out+ 4)) 210
                        (if (%drmp3-hdr-is-ms-stereo hdr o) 2 0)))
           (gain (%drmp3-l3-ldexp-q2 (float (ash 1 (floor +drmp3-max-scfi+ 4)) 1f0) (- +drmp3-max-scfi+ gain-exp))))
      (dotimes (i (+ (gr-n-long-sfb gr) (gr-n-short-sfb gr)))
        (setf (aref scf i) (%drmp3-l3-ldexp-q2 gain (ash (aref iscf i) scf-shift)))))))

(defun %drmp3-l3-pow-43 (x)
  (when (< x 129)
    (return-from %drmp3-l3-pow-43 (aref +drmp3-pow43+ (+ 16 x))))
  (let ((mult 256))
    (when (< x 1024)
      (setf mult 16
            x (ash x 3)))
    (let* ((sign (logand (* 2 x) 64))
           (frac (/ (float (- (logand x 63) sign) 1f0) (float (+ (logand x (lognot 63)) sign) 1f0))))
      (* (* (aref +drmp3-pow43+ (+ 16 (ash (+ x sign) -6)))
            (+ 1f0 (* frac (+ (/ 4f0 3) (* frac (/ 2f0 9))))))
         (float mult 1f0)))))

(defun %drmp3-l3-huffman (dst dst-start bs gr scf layer3gr-limit)
  (declare (type %f32-array dst scf))
  (let* ((buf (drmp3-bs-buf bs))
         (base (drmp3-bs-base bs))
         (one 0f0)
         (ireg 0)
         (big-val-cnt (gr-big-values gr))
         (sfb (gr-sfbtab gr))
         (sfbi (gr-sfbtab-off gr))
         (scfi 0)
         (d dst-start)
         (next (+ base (floor (drmp3-bs-pos bs) 8)))
         (cache (logand (ash (logior (ash (aref buf next) 24) (ash (aref buf (+ next 1)) 16)
                                     (ash (aref buf (+ next 2)) 8) (aref buf (+ next 3)))
                             (logand (drmp3-bs-pos bs) 7))
                        #xffffffff))
         (sh (- (logand (drmp3-bs-pos bs) 7) 8))
         (np 0)
         (tabs +drmp3-tabs+))
    (declare (type (unsigned-byte 32) cache) (type fixnum sh next d))
    (incf next 4)
    (macrolet ((peek-bits (n) `(ash cache (- (- 32 ,n))))
               (flush-bits (n) `(progn (setf cache (logand (ash cache ,n) #xffffffff)) (incf sh ,n)))
               (check-bits () `(loop while (>= sh 0)
                                     do (setf cache (logior cache (logand (ash (aref buf next) sh) #xffffffff)))
                                        (incf next)
                                        (decf sh 8)))
               (bspos () `(+ (- (* (- next base) 8) 24) sh))
               (sign-p () `(logbitp 31 cache)))
      (loop while (> big-val-cnt 0)
            do (let* ((tab-num (aref (gr-table-select gr) ireg))
                      (sfb-cnt (aref (gr-region-count gr) ireg))
                      (codebook (aref +drmp3-tabindex+ tab-num))
                      (linbits (aref +drmp3-linbits+ tab-num)))
                 (incf ireg)
                 (loop
                   (setf np (floor (aref sfb sfbi) 2))
                   (incf sfbi)
                   (let ((pairs-to-decode (min big-val-cnt np)))
                     (setf one (aref scf scfi))
                     (incf scfi)
                     (loop
                       (let* ((w 5)
                              (leaf (aref tabs (+ codebook (peek-bits w)))))
                         (loop while (< leaf 0)
                               do (flush-bits w)
                                  (setf w (logand leaf 7)
                                        leaf (aref tabs (+ codebook (- (peek-bits w) (ash leaf -3))))))
                         (flush-bits (ash leaf -8))
                         (dotimes (j 2)
                           (let ((lsb (logand leaf #x0f)))
                             (if (and (/= linbits 0) (= lsb 15))
                                 (progn
                                   (incf lsb (peek-bits linbits))
                                   (flush-bits linbits)
                                   (check-bits)
                                   (setf (aref dst d) (* (* one (%drmp3-l3-pow-43 lsb)) (if (sign-p) -1f0 1f0))))
                                 (setf (aref dst d) (* (aref +drmp3-pow43+ (- (+ 16 lsb) (* 16 (ash cache -31)))) one)))
                             (flush-bits (if (/= lsb 0) 1 0)))
                           (incf d)
                           (setf leaf (ash leaf -4)))
                         (check-bits))
                       (when (= (decf pairs-to-decode) 0) (return))))
                   (unless (and (> (decf big-val-cnt np) 0) (>= (decf sfb-cnt) 0))
                     (return)))))
      (setf np (- 1 big-val-cnt))
      (block count1
        (loop
          (let* ((codebook-count1 (if (/= (gr-count1-table gr) 0) +drmp3-tab33+ +drmp3-tab32+))
                 (leaf (aref codebook-count1 (peek-bits 4))))
            (unless (logtest leaf 8)
              (setf leaf (aref codebook-count1 (+ (ash leaf -3)
                                                  (ash (logand (ash cache 4) #xffffffff) (- (- 32 (logand leaf 3))))))))
            (flush-bits (logand leaf 7))
            (when (> (bspos) layer3gr-limit)
              (return-from count1))
            (macrolet ((reload-scalefactor ()
                         `(when (= (decf np) 0)
                            (setf np (floor (aref sfb sfbi) 2))
                            (incf sfbi)
                            (when (= np 0) (return-from count1))
                            (setf one (aref scf scfi))
                            (incf scfi)))
                       (deq-count1 (s)
                         `(when (logtest leaf (ash 128 (- ,s)))
                            (setf (aref dst (+ d ,s)) (if (sign-p) (- one) one))
                            (flush-bits 1))))
              (reload-scalefactor)
              (deq-count1 0)
              (deq-count1 1)
              (reload-scalefactor)
              (deq-count1 2)
              (deq-count1 3))
            (check-bits)
            (incf d 4)))))
    (setf (drmp3-bs-pos bs) layer3gr-limit)))

(defun %drmp3-l3-midside-stereo (buf left n)
  (declare (type %f32-array buf) (type fixnum left n))
  (let ((right (+ left 576)))
    (dotimes (i n)
      (let ((a (aref buf (+ left i)))
            (b (aref buf (+ right i))))
        (setf (aref buf (+ left i)) (+ a b)
              (aref buf (+ right i)) (- a b))))))

(defun %drmp3-l3-intensity-stereo-band (buf left n kl kr)
  (declare (type %f32-array buf) (type single-float kl kr))
  (dotimes (i n)
    (setf (aref buf (+ left i 576)) (* (aref buf (+ left i)) kr)
          (aref buf (+ left i)) (* (aref buf (+ left i)) kl))))

(defun %drmp3-l3-stereo-top-band (buf right sfb sfb-off nbands max-band)
  (setf (aref max-band 0) -1 (aref max-band 1) -1 (aref max-band 2) -1)
  (dotimes (i nbands)
    (loop for k from 0 below (aref sfb (+ sfb-off i)) by 2
          do (when (or (/= (aref buf (+ right k)) 0) (/= (aref buf (+ right k 1)) 0))
               (setf (aref max-band (mod i 3)) i)
               (return)))
    (incf right (aref sfb (+ sfb-off i)))))

(defun %drmp3-l3-stereo-process (buf left ist-pos ist-off sfb sfb-off hdr o max-band mpeg2-sh)
  (let ((max-pos (if (%drmp3-hdr-test-mpeg1 hdr o) 7 64)))
    (loop for i from 0
          while (/= (aref sfb (+ sfb-off i)) 0)
          do (let ((ipos (aref ist-pos (+ ist-off i)))
                   (n (aref sfb (+ sfb-off i))))
               (cond ((and (> i (aref max-band (mod i 3))) (< ipos max-pos))
                      (let ((s (if (%drmp3-hdr-test-ms-stereo hdr o) 1.41421356f0 1f0))
                            (kl 0f0) (kr 0f0))
                        (if (%drmp3-hdr-test-mpeg1 hdr o)
                            (setf kl (aref +drmp3-pan+ (* 2 ipos))
                                  kr (aref +drmp3-pan+ (+ (* 2 ipos) 1)))
                            (progn
                              (setf kl 1f0
                                    kr (%drmp3-l3-ldexp-q2 1f0 (ash (ash (+ ipos 1) -1) mpeg2-sh)))
                              (when (logtest ipos 1)
                                (setf kl kr kr 1f0))))
                        (%drmp3-l3-intensity-stereo-band buf left n (* kl s) (* kr s))))
                     ((%drmp3-hdr-test-ms-stereo hdr o)
                      (%drmp3-l3-midside-stereo buf left n)))
               (incf left n)))))

(defun %drmp3-l3-intensity-stereo (buf left ist-pos ist-off gr gr1 hdr o)
  (let* ((max-band (make-array 3 :initial-element -1))
         (n-sfb (+ (gr-n-long-sfb gr) (gr-n-short-sfb gr)))
         (max-blocks (if (/= (gr-n-short-sfb gr) 0) 3 1)))
    (%drmp3-l3-stereo-top-band buf (+ left 576) (gr-sfbtab gr) (gr-sfbtab-off gr) n-sfb max-band)
    (when (/= (gr-n-long-sfb gr) 0)
      (let ((m (max (aref max-band 0) (aref max-band 1) (aref max-band 2))))
        (setf (aref max-band 0) m (aref max-band 1) m (aref max-band 2) m)))
    (dotimes (i max-blocks)
      (let* ((default-pos (if (%drmp3-hdr-test-mpeg1 hdr o) 3 0))
             (itop (+ (- n-sfb max-blocks) i))
             (prev (- itop max-blocks)))
        (setf (aref ist-pos (+ ist-off itop))
              (if (>= (aref max-band i) prev) default-pos (aref ist-pos (+ ist-off prev))))))
    (%drmp3-l3-stereo-process buf left ist-pos ist-off (gr-sfbtab gr) (gr-sfbtab-off gr) hdr o max-band
                              (logand (gr-scalefac-compress gr1) 1))))

(defun %drmp3-l3-reorder (grbuf start scratch sfb sfb-off)
  (let ((src start) (dst 0))
    (loop for len = (aref sfb sfb-off)
          until (= len 0)
          do (dotimes (i len)
               (setf (aref scratch dst) (aref grbuf src)
                     (aref scratch (+ dst 1)) (aref grbuf (+ src len))
                     (aref scratch (+ dst 2)) (aref grbuf (+ src (* 2 len))))
               (incf dst 3)
               (incf src))
             (incf sfb-off 3)
             (incf src (* 2 len)))
    (replace grbuf scratch :start1 start :end2 dst)))

(defun %drmp3-l3-antialias (grbuf start nbands)
  (declare (type %f32-array grbuf))
  (let ((aa +drmp3-aa+))
    (loop while (> nbands 0)
          do (dotimes (i 8)
               (let ((u (aref grbuf (+ start 18 i)))
                     (d (aref grbuf (+ start (- 17 i)))))
                 (setf (aref grbuf (+ start 18 i)) (- (* u (aref aa i)) (* d (aref aa (+ 8 i))))
                       (aref grbuf (+ start (- 17 i))) (+ (* u (aref aa (+ 8 i))) (* d (aref aa i))))))
             (decf nbands)
             (incf start 18))))

(defun %drmp3-l3-dct3-9 (y)
  (declare (type (simple-array single-float (9)) y))
  (let* ((s0 (aref y 0)) (s2 (aref y 2)) (s4 (aref y 4)) (s6 (aref y 6)) (s8 (aref y 8))
         (t0 (+ s0 (* s6 0.5f0))) (t2 0f0) (t4 0f0) s1 s3 s5 s7)
    (setf s0 (- s0 s6)
          t4 (* (+ s4 s2) 0.93969262f0)
          t2 (* (+ s8 s2) 0.76604444f0)
          s6 (* (- s4 s8) 0.17364818f0)
          s4 (+ s4 (- s8 s2))
          s2 (- s0 (* s4 0.5f0)))
    (setf (aref y 4) (+ s4 s0))
    (setf s8 (+ (- t0 t2) s6)
          s0 (+ (- t0 t4) t2)
          s4 (- (+ t0 t4) s6))
    (setf s1 (aref y 1) s3 (aref y 3) s5 (aref y 5) s7 (aref y 7))
    (setf s3 (* s3 0.86602540f0)
          t0 (* (+ s5 s1) 0.98480775f0)
          t4 (* (- s5 s7) 0.34202014f0)
          t2 (* (+ s1 s7) 0.64278761f0)
          s1 (* (- (- s1 s5) s7) 0.86602540f0)
          s5 (- (- t0 s3) t2)
          s7 (- (- t4 s3) t0)
          s3 (- (+ t4 s3) t2))
    (setf (aref y 0) (- s4 s7)
          (aref y 1) (+ s2 s1)
          (aref y 2) (- s0 s3)
          (aref y 3) (+ s8 s5)
          (aref y 5) (- s8 s5)
          (aref y 6) (+ s0 s3)
          (aref y 7) (- s2 s1)
          (aref y 8) (+ s4 s7))))

(defun %drmp3-l3-imdct36 (grbuf g overlap ov window w-off nbands)
  (declare (type %f32-array grbuf overlap window))
  (let ((co (make-array 9 :element-type 'single-float :initial-element 0f0))
        (si (make-array 9 :element-type 'single-float :initial-element 0f0))
        (twid9 +drmp3-twid9+))
    (dotimes (j nbands)
      (setf (aref co 0) (- (aref grbuf g))
            (aref si 0) (aref grbuf (+ g 17)))
      (dotimes (i 4)
        (setf (aref si (- 8 (* 2 i))) (- (aref grbuf (+ g (* 4 i) 1)) (aref grbuf (+ g (* 4 i) 2)))
              (aref co (+ 1 (* 2 i))) (+ (aref grbuf (+ g (* 4 i) 1)) (aref grbuf (+ g (* 4 i) 2)))
              (aref si (- 7 (* 2 i))) (- (aref grbuf (+ g (* 4 i) 4)) (aref grbuf (+ g (* 4 i) 3)))
              (aref co (+ 2 (* 2 i))) (- (+ (aref grbuf (+ g (* 4 i) 3)) (aref grbuf (+ g (* 4 i) 4))))))
      (%drmp3-l3-dct3-9 co)
      (%drmp3-l3-dct3-9 si)
      (setf (aref si 1) (- (aref si 1))
            (aref si 3) (- (aref si 3))
            (aref si 5) (- (aref si 5))
            (aref si 7) (- (aref si 7)))
      (dotimes (i 9)
        (let ((ovl (aref overlap (+ ov i)))
              (sum (+ (* (aref co i) (aref twid9 (+ 9 i))) (* (aref si i) (aref twid9 i)))))
          (setf (aref overlap (+ ov i)) (- (* (aref co i) (aref twid9 i)) (* (aref si i) (aref twid9 (+ 9 i)))))
          (setf (aref grbuf (+ g i)) (- (* ovl (aref window (+ w-off i))) (* sum (aref window (+ w-off 9 i))))
                (aref grbuf (+ g (- 17 i))) (+ (* ovl (aref window (+ w-off 9 i))) (* sum (aref window (+ w-off i)))))))
      (incf g 18)
      (incf ov 9))))

(defun %drmp3-l3-idct3 (x0 x1 x2 dst)
  (let ((m1 (* x1 0.86602540f0))
        (a1 (- x0 (* x2 0.5f0))))
    (setf (aref dst 1) (+ x0 x2)
          (aref dst 0) (+ a1 m1)
          (aref dst 2) (- a1 m1))))

(defun %drmp3-l3-imdct12 (x xo dst d overlap ov)
  (let ((co (make-array 3 :element-type 'single-float :initial-element 0f0))
        (si (make-array 3 :element-type 'single-float :initial-element 0f0))
        (tw +drmp3-twid3+))
    (%drmp3-l3-idct3 (- (aref x xo)) (+ (aref x (+ xo 6)) (aref x (+ xo 3))) (+ (aref x (+ xo 12)) (aref x (+ xo 9))) co)
    (%drmp3-l3-idct3 (aref x (+ xo 15)) (- (aref x (+ xo 12)) (aref x (+ xo 9))) (- (aref x (+ xo 6)) (aref x (+ xo 3))) si)
    (setf (aref si 1) (- (aref si 1)))
    (dotimes (i 3)
      (let ((ovl (aref overlap (+ ov i)))
            (sum (+ (* (aref co i) (aref tw (+ 3 i))) (* (aref si i) (aref tw i)))))
        (setf (aref overlap (+ ov i)) (- (* (aref co i) (aref tw i)) (* (aref si i) (aref tw (+ 3 i)))))
        (setf (aref dst (+ d i)) (- (* ovl (aref tw (- 2 i))) (* sum (aref tw (- 5 i))))
              (aref dst (+ d (- 5 i))) (+ (* ovl (aref tw (- 5 i))) (* sum (aref tw (- 2 i)))))))))

(defun %drmp3-l3-imdct-short (grbuf g overlap ov nbands)
  (let ((tmp (make-array 18 :element-type 'single-float :initial-element 0f0)))
    (loop while (> nbands 0)
          do (replace tmp grbuf :start2 g :end2 (+ g 18))
             (replace grbuf overlap :start1 g :start2 ov :end2 (+ ov 6))
             (%drmp3-l3-imdct12 tmp 0 grbuf (+ g 6) overlap (+ ov 6))
             (%drmp3-l3-imdct12 tmp 1 grbuf (+ g 12) overlap (+ ov 6))
             (%drmp3-l3-imdct12 tmp 2 overlap ov overlap (+ ov 6))
             (decf nbands)
             (incf ov 9)
             (incf g 18))))

(defun %drmp3-l3-change-sign (grbuf g)
  (incf g 18)
  (loop for b from 0 below 32 by 2
        do (loop for i from 1 below 18 by 2
                 do (setf (aref grbuf (+ g i)) (- (aref grbuf (+ g i)))))
           (incf g 36)))

(defun %drmp3-l3-imdct-gr (grbuf g overlap ov block-type n-long-bands)
  (when (/= n-long-bands 0)
    (%drmp3-l3-imdct36 grbuf g overlap ov +drmp3-mdct-window+ 0 n-long-bands)
    (incf g (* 18 n-long-bands))
    (incf ov (* 9 n-long-bands)))
  (if (= block-type +drmp3-short-block-type+)
      (%drmp3-l3-imdct-short grbuf g overlap ov (- 32 n-long-bands))
      (%drmp3-l3-imdct36 grbuf g overlap ov +drmp3-mdct-window+ (if (= block-type +drmp3-stop-block-type+) 18 0)
                         (- 32 n-long-bands))))

(defun %drmp3-l3-save-reservoir (dec)
  (let* ((bs (drmp3dec-bs dec))
         (pos (floor (+ (drmp3-bs-pos bs) 7) 8))
         (remains (- (floor (drmp3-bs-limit bs) 8) pos)))
    (when (> remains +drmp3-max-bitreservoir-bytes+)
      (incf pos (- remains +drmp3-max-bitreservoir-bytes+))
      (setf remains +drmp3-max-bitreservoir-bytes+))
    (when (> remains 0)
      (replace (drmp3dec-reserv-buf dec) (drmp3dec-maindata dec) :start2 pos :end2 (+ pos remains)))
    (setf (drmp3dec-reserv dec) remains)))

(defun %drmp3-l3-restore-reservoir (dec bs main-data-begin)
  (let* ((frame-bytes (floor (- (drmp3-bs-limit bs) (drmp3-bs-pos bs)) 8))
         (bytes-have (min (drmp3dec-reserv dec) main-data-begin))
         (maindata (drmp3dec-maindata dec)))
    (replace maindata (drmp3dec-reserv-buf dec) :start2 (max 0 (- (drmp3dec-reserv dec) main-data-begin))
                                                :end2 (+ (max 0 (- (drmp3dec-reserv dec) main-data-begin))
                                                         (min (drmp3dec-reserv dec) main-data-begin)))
    (let ((src (+ (drmp3-bs-base bs) (floor (drmp3-bs-pos bs) 8))))
      (replace maindata (drmp3-bs-buf bs) :start1 bytes-have :start2 src :end2 (+ src frame-bytes)))
    (setf (drmp3dec-bs dec) (%make-drmp3-bs maindata 0 0 (* (+ bytes-have frame-bytes) 8)))
    (>= (drmp3dec-reserv dec) main-data-begin)))

(defun %drmp3-l3-decode (dec gr-base nch)
  (let ((hdr (drmp3dec-header dec))
        (grbuf (drmp3dec-grbuf dec))
        (gr-infos (drmp3dec-gr-info dec)))
    (dotimes (ch nch)
      (let* ((gr (svref gr-infos (+ gr-base ch)))
             (layer3gr-limit (+ (drmp3-bs-pos (drmp3dec-bs dec)) (gr-part-23-length gr))))
        (%drmp3-l3-decode-scalefactors hdr 0 (drmp3dec-ist-pos dec) (* ch 39) (drmp3dec-bs dec) gr (drmp3dec-scf dec) ch)
        (%drmp3-l3-huffman grbuf (* ch 576) (drmp3dec-bs dec) gr (drmp3dec-scf dec) layer3gr-limit)))
    (cond ((%drmp3-hdr-test-i-stereo hdr 0)
           (%drmp3-l3-intensity-stereo grbuf 0 (drmp3dec-ist-pos dec) 39 (svref gr-infos gr-base)
                                       (svref gr-infos (+ gr-base 1)) hdr 0))
          ((%drmp3-hdr-is-ms-stereo hdr 0)
           (%drmp3-l3-midside-stereo grbuf 0 576)))
    (dotimes (ch nch)
      (let* ((gr (svref gr-infos (+ gr-base ch)))
             (aa-bands 31)
             (n-long-bands (ash (if (/= (gr-mixed-block-flag gr) 0) 2 0)
                                (if (= (%drmp3-hdr-get-my-sample-rate hdr 0) 2) 1 0))))
        (when (/= (gr-n-short-sfb gr) 0)
          (setf aa-bands (- n-long-bands 1))
          (%drmp3-l3-reorder grbuf (+ (* ch 576) (* n-long-bands 18)) (drmp3dec-syn dec)
                             (gr-sfbtab gr) (+ (gr-sfbtab-off gr) (gr-n-long-sfb gr))))
        (%drmp3-l3-antialias grbuf (* ch 576) aa-bands)
        (%drmp3-l3-imdct-gr grbuf (* ch 576) (drmp3dec-mdct-overlap dec) (* ch 288) (gr-block-type gr) n-long-bands)
        (%drmp3-l3-change-sign grbuf (* ch 576))))))

;;;----------------------------------------------------------------------------------
;;; Synthesis
;;;----------------------------------------------------------------------------------

;; drmp3d_DCT_II() with the SIMD summation order
(defun %drmp3d-dct-ii (grbuf start n)
  (declare (type %f32-array grbuf) (type fixnum start n))
  (let ((tt (make-array 32 :element-type 'single-float :initial-element 0f0))
        (sec +drmp3-sec+))
    (declare (type (simple-array single-float (32)) tt))
    (dotimes (k n)
      (let ((y (+ start k)))
        (dotimes (i 8)
          (let* ((x0 (aref grbuf (+ y (* i 18))))
                 (x1 (aref grbuf (+ y (* (- 15 i) 18))))
                 (x2 (aref grbuf (+ y (* (+ 16 i) 18))))
                 (x3 (aref grbuf (+ y (* (- 31 i) 18))))
                 (t0 (+ x0 x3))
                 (t1 (+ x1 x2))
                 (t2 (* (- x1 x2) (aref sec (* 3 i))))
                 (t3 (* (- x0 x3) (aref sec (+ (* 3 i) 1)))))
            (setf (aref tt i) (+ t0 t1)
                  (aref tt (+ 8 i)) (* (- t0 t1) (aref sec (+ (* 3 i) 2)))
                  (aref tt (+ 16 i)) (+ t3 t2)
                  (aref tt (+ 24 i)) (* (- t3 t2) (aref sec (+ (* 3 i) 2))))))
        (loop for x from 0 below 32 by 8
              do (let ((x0 (aref tt x)) (x1 (aref tt (+ x 1))) (x2 (aref tt (+ x 2))) (x3 (aref tt (+ x 3)))
                       (x4 (aref tt (+ x 4))) (x5 (aref tt (+ x 5))) (x6 (aref tt (+ x 6))) (x7 (aref tt (+ x 7)))
                       (xt 0f0))
                   (declare (type single-float x0 x1 x2 x3 x4 x5 x6 x7 xt))
                   (setf xt (- x0 x7) x0 (+ x0 x7))
                   (setf x7 (- x1 x6) x1 (+ x1 x6))
                   (setf x6 (- x2 x5) x2 (+ x2 x5))
                   (setf x5 (- x3 x4) x3 (+ x3 x4))
                   (setf x4 (- x0 x3) x0 (+ x0 x3))
                   (setf x3 (- x1 x2) x1 (+ x1 x2))
                   (setf (aref tt x) (+ x0 x1)
                         (aref tt (+ x 4)) (* (- x0 x1) 0.70710677f0))
                   (setf x5 (+ x5 x6)
                         x6 (* (+ x6 x7) 0.70710677f0)
                         x7 (+ x7 xt)
                         x3 (* (+ x3 x4) 0.70710677f0))
                   (setf x5 (- x5 (* x7 0.198912367f0)))   ; Rotate by PI/8
                   (setf x7 (+ x7 (* x5 0.382683432f0)))
                   (setf x5 (- x5 (* x7 0.198912367f0)))
                   (setf x0 (- xt x6) xt (+ xt x6))
                   (setf (aref tt (+ x 1)) (* (+ xt x7) 0.50979561f0)
                         (aref tt (+ x 2)) (* (+ x4 x3) 0.54119611f0)
                         (aref tt (+ x 3)) (* (- x0 x5) 0.60134488f0)
                         (aref tt (+ x 5)) (* (+ x0 x5) 0.89997619f0)
                         (aref tt (+ x 6)) (* (- x4 x3) 1.30656302f0)
                         (aref tt (+ x 7)) (* (- xt x7) 2.56291556f0))))
        (dotimes (i 7)
          (let ((s (+ (aref tt (+ 24 i)) (aref tt (+ 24 i 1)))))
            (setf (aref grbuf y) (aref tt i)
                  (aref grbuf (+ y 18)) (+ (aref tt (+ 16 i)) s)
                  (aref grbuf (+ y 36)) (+ (aref tt (+ 8 i)) (aref tt (+ 8 i 1)))
                  (aref grbuf (+ y 54)) (+ (aref tt (+ 16 i 1)) s)))
          (incf y (* 4 18)))
        (setf (aref grbuf y) (aref tt 7)
              (aref grbuf (+ y 18)) (+ (aref tt 23) (aref tt 31))
              (aref grbuf (+ y 36)) (aref tt 15)
              (aref grbuf (+ y 54)) (aref tt 31))))))

;; drmp3d_scale_pcm(), scalar version
(declaim (inline %drmp3d-scale-pcm %drmp3d-scale-pcm-sse))
(defun %drmp3d-scale-pcm (sample)
  (declare (type single-float sample))
  (cond ((>= sample 32766.5f0) 32767)
        ((<= sample -32767.5f0) -32768)
        (t (let ((s (%i16 (truncate (+ sample 0.5f0)))))
             (if (< s 0) (- s 1) s)))))

;; _mm_cvtps_epi32(_mm_max_ps(_mm_min_ps(x, 32767), -32768)) + _mm_packs_epi32()
(defun %drmp3d-scale-pcm-sse (sample)
  (declare (type single-float sample))
  (round (max (min sample 32767f0) -32768f0)))

(defun %drmp3d-synth-pair (pcm p nch z zi)
  (declare (type (simple-array (signed-byte 16) (*)) pcm) (type %f32-array z) (type fixnum p nch zi))
  (macrolet ((z (k) `(aref z (+ zi (* ,k 64)))))
    (let ((a (* (- (z 14) (z 0)) 29f0)))
      (setf a (+ a (* (+ (z 1) (z 13)) 213f0)))
      (setf a (+ a (* (- (z 12) (z 2)) 459f0)))
      (setf a (+ a (* (+ (z 3) (z 11)) 2037f0)))
      (setf a (+ a (* (- (z 10) (z 4)) 5153f0)))
      (setf a (+ a (* (+ (z 5) (z 9)) 6574f0)))
      (setf a (+ a (* (- (z 8) (z 6)) 37489f0)))
      (setf a (+ a (* (z 7) 75038f0)))
      (setf (aref pcm p) (%drmp3d-scale-pcm a)))
    (incf zi 2)
    (let ((a (* (z 14) 104f0)))
      (setf a (+ a (* (z 12) 1567f0)))
      (setf a (+ a (* (z 10) 9727f0)))
      (setf a (+ a (* (z 8) 64019f0)))
      (setf a (+ a (* (z 6) -9975f0)))
      (setf a (+ a (* (z 4) -45f0)))
      (setf a (+ a (* (z 2) 146f0)))
      (setf a (+ a (* (z 0) -5f0)))
      (setf (aref pcm (+ p (* 16 nch))) (%drmp3d-scale-pcm a)))))

(defun %drmp3d-synth (grbuf xl dstl-pcm dstl nch lins li)
  (declare (type %f32-array grbuf lins) (type (simple-array (signed-byte 16) (*)) dstl-pcm)
           (type fixnum xl dstl nch li)
           (optimize speed (safety 0)))
  (let* ((xr (+ xl (* 576 (- nch 1))))
         (dstr (+ dstl (- nch 1)))
         (zlin (+ li (* 15 64)))
         (win +drmp3-win+)
         (pcm dstl-pcm)
         (a (make-array 4 :element-type 'single-float :initial-element 0f0))
         (b (make-array 4 :element-type 'single-float :initial-element 0f0)))
    (declare (type fixnum xr dstr zlin) (type %f32-array win)
             (type (simple-array single-float (4)) a b))
    (setf (aref lins (+ zlin (* 4 15))) (aref grbuf (+ xl (* 18 16)))
          (aref lins (+ zlin (* 4 15) 1)) (aref grbuf (+ xr (* 18 16)))
          (aref lins (+ zlin (* 4 15) 2)) (aref grbuf xl)
          (aref lins (+ zlin (* 4 15) 3)) (aref grbuf xr)
          (aref lins (+ zlin (* 4 31))) (aref grbuf (+ xl 1 (* 18 16)))
          (aref lins (+ zlin (* 4 31) 1)) (aref grbuf (+ xr 1 (* 18 16)))
          (aref lins (+ zlin (* 4 31) 2)) (aref grbuf (+ xl 1))
          (aref lins (+ zlin (* 4 31) 3)) (aref grbuf (+ xr 1)))
    (%drmp3d-synth-pair pcm dstr nch lins (+ li (* 4 15) 1))
    (%drmp3d-synth-pair pcm (+ dstr (* 32 nch)) nch lins (+ li (* 4 15) 64 1))
    (%drmp3d-synth-pair pcm dstl nch lins (+ li (* 4 15)))
    (%drmp3d-synth-pair pcm (+ dstl (* 32 nch)) nch lins (+ li (* 4 15) 64))
    (loop with w of-type fixnum = 0
          for i of-type fixnum from 14 downto 0
          do (progn
               (setf (aref lins (+ zlin (* 4 i))) (aref grbuf (+ xl (* 18 (- 31 i))))
                     (aref lins (+ zlin (* 4 i) 1)) (aref grbuf (+ xr (* 18 (- 31 i))))
                     (aref lins (+ zlin (* 4 i) 2)) (aref grbuf (+ xl 1 (* 18 (- 31 i))))
                     (aref lins (+ zlin (* 4 i) 3)) (aref grbuf (+ xr 1 (* 18 (- 31 i))))
                     (aref lins (+ zlin (* 4 i) 64)) (aref grbuf (+ xl 1 (* 18 (+ 1 i))))
                     (aref lins (+ zlin (* 4 i) 64 1)) (aref grbuf (+ xr 1 (* 18 (+ 1 i))))
                     (aref lins (+ zlin (- (* 4 i) 64) 2)) (aref grbuf (+ xl (* 18 (+ 1 i))))
                     (aref lins (+ zlin (- (* 4 i) 64) 3)) (aref grbuf (+ xr (* 18 (+ 1 i)))))
               (macrolet ((synth-step (k mode)
                            `(let* ((w0 (aref win w))
                                    (w1 (aref win (+ w 1)))
                                    (vz (+ zlin (* 4 i) (- (* 64 ,k))))
                                    (vy (+ zlin (* 4 i) (- (* 64 (- 15 ,k))))))
                               (incf w 2)
                               (dotimes (j 4)
                                 (let ((z (aref lins (+ vz j)))
                                       (y (aref lins (+ vy j))))
                                   ,(ecase mode
                                      (0 '(setf (aref b j) (+ (* z w1) (* y w0))
                                                (aref a j) (- (* z w0) (* y w1))))
                                      (1 '(setf (aref b j) (+ (aref b j) (+ (* z w1) (* y w0)))
                                                (aref a j) (+ (aref a j) (- (* z w0) (* y w1)))))
                                      (2 '(setf (aref b j) (+ (aref b j) (+ (* z w1) (* y w0)))
                                                (aref a j) (+ (aref a j) (- (* y w1) (* z w0)))))))))))
                 (synth-step 0 0) (synth-step 1 2) (synth-step 2 1) (synth-step 3 2) (synth-step 4 1) (synth-step 5 2) (synth-step 6 1) (synth-step 7 2)))
             (setf (aref pcm (+ dstr (* (- 15 i) nch))) (%drmp3d-scale-pcm-sse (aref a 1))
                   (aref pcm (+ dstr (* (+ 17 i) nch))) (%drmp3d-scale-pcm-sse (aref b 1))
                   (aref pcm (+ dstl (* (- 15 i) nch))) (%drmp3d-scale-pcm-sse (aref a 0))
                   (aref pcm (+ dstl (* (+ 17 i) nch))) (%drmp3d-scale-pcm-sse (aref b 0))
                   (aref pcm (+ dstr (* (- 47 i) nch))) (%drmp3d-scale-pcm-sse (aref a 3))
                   (aref pcm (+ dstr (* (+ 49 i) nch))) (%drmp3d-scale-pcm-sse (aref b 3))
                   (aref pcm (+ dstl (* (- 47 i) nch))) (%drmp3d-scale-pcm-sse (aref a 2))
                   (aref pcm (+ dstl (* (+ 49 i) nch))) (%drmp3d-scale-pcm-sse (aref b 2)))
             )))

(defun %drmp3d-synth-granule (qmf-state grbuf nbands nch pcm p lins)
  (dotimes (i nch)
    (%drmp3d-dct-ii grbuf (* 576 i) nbands))
  (replace lins qmf-state :end2 (* 15 64))
  (loop for i from 0 below nbands by 2
        do (%drmp3d-synth grbuf i pcm (+ p (* 32 nch i)) nch lins (* i 64)))
  (if (= nch 1)
      (loop for i from 0 below (* 15 64) by 2
            do (setf (aref qmf-state i) (aref lins (+ (* nbands 64) i))))
      (replace qmf-state lins :start2 (* nbands 64) :end2 (+ (* nbands 64) (* 15 64)))))

;;;----------------------------------------------------------------------------------
;;; Frame decoding
;;;----------------------------------------------------------------------------------

(defun %drmp3d-match-frame (h o mp3-bytes frame-bytes)
  (let ((i 0))
    (dotimes (nmatch +drmp3-max-frame-sync-matches+ t)
      (incf i (+ (%drmp3-hdr-frame-bytes h (+ o i) frame-bytes) (%drmp3-hdr-padding h (+ o i))))
      (when (> (+ i +drmp3-hdr-size+) mp3-bytes)
        (return (> nmatch 0)))
      (unless (%drmp3-hdr-compare h o h (+ o i))
        (return nil)))))

;; Returns (values offset frame-bytes), updates the free format bytes of DEC
(defun %drmp3d-find-frame (mp3 o mp3-bytes dec)
  (dotimes (i (- mp3-bytes +drmp3-hdr-size+))
    (let ((p (+ o i)))
      (when (%drmp3-hdr-valid mp3 p)
        (let* ((frame-bytes (%drmp3-hdr-frame-bytes mp3 p (drmp3dec-free-format-bytes dec)))
               (frame-and-padding (+ frame-bytes (%drmp3-hdr-padding mp3 p))))
          (loop for k from +drmp3-hdr-size+
                while (and (= frame-bytes 0) (< k +drmp3-max-free-format-frame-size+)
                           (< (+ i (* 2 k)) (- mp3-bytes +drmp3-hdr-size+)))
                do (when (%drmp3-hdr-compare mp3 p mp3 (+ p k))
                     (let* ((fb (- k (%drmp3-hdr-padding mp3 p)))
                            (nextfb (+ fb (%drmp3-hdr-padding mp3 (+ p k)))))
                       (unless (or (> (+ i k nextfb +drmp3-hdr-size+) mp3-bytes)
                                   (not (%drmp3-hdr-compare mp3 p mp3 (+ p k nextfb))))
                         (setf frame-and-padding k
                               frame-bytes fb
                               (drmp3dec-free-format-bytes dec) fb)))))
          (when (or (and (/= frame-bytes 0) (<= (+ i frame-and-padding) mp3-bytes)
                         (%drmp3d-match-frame mp3 p (- mp3-bytes i) frame-bytes))
                    (and (= i 0) (= frame-and-padding mp3-bytes)))
            (return-from %drmp3d-find-frame (values i frame-and-padding)))
          (setf (drmp3dec-free-format-bytes dec) 0)))))
  (values mp3-bytes 0))

;; drmp3dec_decode_frame(), PCM is a (signed-byte 16) array or NIL
;; Returns (values samples frame-bytes channels sample-rate layer bitrate-kbps)
(defun drmp3dec-decode-frame (dec mp3 o mp3-bytes pcm)
  (let ((i 0) (frame-size 0) (success t) (frame-bytes 0)
        (channels 0) (sample-rate 0) (layer 0) (bitrate 0))
    (when (and (> mp3-bytes 4) (= (aref (drmp3dec-header dec) 0) #xff)
               (%drmp3-hdr-compare (drmp3dec-header dec) 0 mp3 o))
      (setf frame-size (+ (%drmp3-hdr-frame-bytes mp3 o (drmp3dec-free-format-bytes dec)) (%drmp3-hdr-padding mp3 o)))
      (when (and (/= frame-size mp3-bytes)
                 (or (> (+ frame-size +drmp3-hdr-size+) mp3-bytes)
                     (not (%drmp3-hdr-compare mp3 o mp3 (+ o frame-size)))))
        (setf frame-size 0)))
    (when (= frame-size 0)
      (drmp3dec-zero dec)
      (multiple-value-setq (i frame-size) (%drmp3d-find-frame mp3 o mp3-bytes dec))
      (when (or (= frame-size 0) (> (+ i frame-size) mp3-bytes))
        (return-from drmp3dec-decode-frame (values 0 i 0 0 0 0))))
    (let* ((h (+ o i))
           (hdr (drmp3dec-header dec)))
      (replace hdr mp3 :start2 h :end2 (+ h +drmp3-hdr-size+))
      (setf frame-bytes (+ i frame-size)
            channels (if (%drmp3-hdr-is-mono mp3 h) 1 2)
            sample-rate (%drmp3-hdr-sample-rate-hz mp3 h)
            layer (- 4 (%drmp3-hdr-get-layer mp3 h))
            bitrate (%drmp3-hdr-bitrate-kbps mp3 h))
      (let ((bs-frame (%make-drmp3-bs mp3 (+ h +drmp3-hdr-size+) 0 (* (- frame-size +drmp3-hdr-size+) 8))))
        (when (%drmp3-hdr-is-crc mp3 h)
          (%drmp3-bs-get-bits bs-frame 16))
        (if (= layer 3)
            (let ((main-data-begin (%drmp3-l3-read-side-info bs-frame (drmp3dec-gr-info dec) mp3 h)))
              (when (or (< main-data-begin 0) (> (drmp3-bs-pos bs-frame) (drmp3-bs-limit bs-frame)))
                (drmp3dec-init dec)
                (return-from drmp3dec-decode-frame (values 0 frame-bytes channels sample-rate layer bitrate)))
              (setf success (%drmp3-l3-restore-reservoir dec bs-frame main-data-begin))
              (when (and success pcm)
                (let ((p 0))
                  (dotimes (igr (if (%drmp3-hdr-test-mpeg1 mp3 h) 2 1))
                    (fill (drmp3dec-grbuf dec) 0f0)
                    (%drmp3-l3-decode dec (* igr channels) channels)
                    (%drmp3d-synth-granule (drmp3dec-qmf-state dec) (drmp3dec-grbuf dec) 18 channels pcm p (drmp3dec-syn dec))
                    (incf p (* 576 channels)))))
              (%drmp3-l3-save-reservoir dec))
            (let ((sci (make-drmp3-l12-scale-info)))
              (unless pcm
                (return-from drmp3dec-decode-frame
                  (values (%drmp3-hdr-frame-samples mp3 h) frame-bytes channels sample-rate layer bitrate)))
              (%drmp3-l12-read-scale-info mp3 h bs-frame sci)
              (fill (drmp3dec-grbuf dec) 0f0)
              (let ((ii 0) (p 0))
                (dotimes (igr 3)
                  (when (= 12 (incf ii (%drmp3-l12-dequantize-granule (drmp3dec-grbuf dec) ii bs-frame sci (logior layer 1))))
                    (setf ii 0)
                    (%drmp3-l12-apply-scf-384 sci igr (drmp3dec-grbuf dec))
                    (%drmp3d-synth-granule (drmp3dec-qmf-state dec) (drmp3dec-grbuf dec) 12 channels pcm p (drmp3dec-syn dec))
                    (fill (drmp3dec-grbuf dec) 0f0)
                    (incf p (* 384 channels)))
                  (when (> (drmp3-bs-pos bs-frame) (drmp3-bs-limit bs-frame))
                    (drmp3dec-init dec)
                    (return-from drmp3dec-decode-frame (values 0 frame-bytes channels sample-rate layer bitrate))))))))
      (values (if success (%drmp3-hdr-frame-samples hdr 0) 0) frame-bytes channels sample-rate layer bitrate))))

;;;----------------------------------------------------------------------------------
;;; Main Public API
;;;----------------------------------------------------------------------------------

(defstruct (drmp3 (:constructor %make-drmp3))
  (decoder (%make-drmp3dec))
  (channels 0)
  (sample-rate 0)
  (data nil)                                    ; Memory stream
  (data-size 0)
  (current-read-pos 0)
  (stream-cursor 0)
  (stream-length +drmp3-uint64-max+)
  (stream-start-offset 0)
  (delay-in-pcm-frames 0)
  (padding-in-pcm-frames 0)
  (total-pcm-frame-count +drmp3-uint64-max+)
  (current-pcm-frame 0)
  (pcm-frames-consumed-in-mp3-frame 0)
  (pcm-frames-remaining-in-mp3-frame 0)
  (mp3-frame-channels 0)
  (mp3-frame-sample-rate 0)
  (pcm-frames (make-array +drmp3-max-samples-per-frame+ :element-type '(signed-byte 16) :initial-element 0))
  (is-vbr nil)
  (is-cbr nil))

;; drmp3_decode_next_frame_ex__memory(), returns (values pcm-frames-read frame-bytes frame-data-offset)
(defun %drmp3-decode-next-frame-ex (mp3 pcm)
  (let ((pcm-frames-read 0) (frame-bytes 0) (frame-data nil))
    (loop
      (multiple-value-bind (samples bytes channels sample-rate)
          (drmp3dec-decode-frame (drmp3-decoder mp3) (drmp3-data mp3) (drmp3-current-read-pos mp3)
                                 (- (drmp3-data-size mp3) (drmp3-current-read-pos mp3)) pcm)
        (setf frame-bytes bytes)
        (cond ((> samples 0)
               (setf pcm-frames-read (%drmp3-hdr-frame-samples (drmp3dec-header (drmp3-decoder mp3)) 0)
                     (drmp3-pcm-frames-consumed-in-mp3-frame mp3) 0
                     (drmp3-pcm-frames-remaining-in-mp3-frame mp3) pcm-frames-read
                     (drmp3-mp3-frame-channels mp3) channels
                     (drmp3-mp3-frame-sample-rate mp3) sample-rate
                     frame-data (drmp3-current-read-pos mp3))
               (return))
              ((> bytes 0)
               ;; No frames were read, but it looks like we skipped past one. Read the next MP3 frame
               (incf (drmp3-current-read-pos mp3) bytes)
               (incf (drmp3-stream-cursor mp3) bytes))
              (t (return)))))   ; Nothing at all was read. Abort
    ;; Consume the data
    (incf (drmp3-current-read-pos mp3) frame-bytes)
    (incf (drmp3-stream-cursor mp3) frame-bytes)
    (values pcm-frames-read frame-bytes frame-data)))

(defun %drmp3-decode-next-frame (mp3)
  (%drmp3-decode-next-frame-ex mp3 (drmp3-pcm-frames mp3)))

(defun %drmp3-seek-memory (mp3 offset origin)
  (let ((new-cursor (+ offset (ecase origin (:set 0) (:cur (drmp3-current-read-pos mp3)) (:end (drmp3-data-size mp3))))))
    (when (and (>= new-cursor 0) (<= new-cursor (drmp3-data-size mp3)))
      (setf (drmp3-current-read-pos mp3) new-cursor)
      t)))

(defun %drmp3-read-memory (mp3 n)
  (let* ((pos (drmp3-current-read-pos mp3))
         (count (max 0 (min n (- (drmp3-data-size mp3) pos)))))
    (incf (drmp3-current-read-pos mp3) count)
    (subseq (drmp3-data mp3) pos (+ pos count))))

;; drmp3_init_memory() -> drmp3_init_internal()
(defun drmp3-init-memory (data &optional (data-size (length data)))
  "Initialize an MP3 decoder from data in memory, returns NIL on failure"
  (when (or (null data) (= data-size 0))
    (return-from drmp3-init-memory nil))
  (let ((mp3 (%make-drmp3 :data data :data-size data-size))
        (detected-mp3-frame-count #xffffffff))
    (drmp3dec-init (drmp3-decoder mp3))
    ;; We'll first check for any ID3v1 or APE tags
    (when (%drmp3-seek-memory mp3 0 :end)
      (let ((stream-len (drmp3-current-read-pos mp3))
            (stream-end-offset 0))
        ;; ID3v1
        (when (> stream-len 128)
          (when (%drmp3-seek-memory mp3 (- stream-end-offset 128) :end)
            (let ((id3 (%drmp3-read-memory mp3 3)))
              (when (and (= (length id3) 3) (equalp id3 (map 'vector #'char-code "TAG")))
                (decf stream-end-offset 128)
                (decf stream-len 128)))))
        ;; APE
        (when (> stream-len 32)
          (when (%drmp3-seek-memory mp3 (- stream-end-offset 32) :end)
            (let ((ape (%drmp3-read-memory mp3 32)))
              (when (and (= (length ape) 32) (equalp (subseq ape 0 8) (map 'vector #'char-code "APETAGEX")))
                ;; NOTE: char values are sign extended before the conversion to drmp3_uint32
                (flet ((sx (b) (logand (if (>= b 128) (- b 256) b) #xffffffff)))
                  (let ((tag-size (logior (sx (aref ape 24)) (ash (sx (aref ape 25)) 8)
                                          (ash (sx (aref ape 26)) 16) (ash (sx (aref ape 27)) 24))))
                    (setf tag-size (logand tag-size #xffffffff))
                    (when (< (logand (+ 32 tag-size) #xffffffff) stream-len)
                      (decf stream-end-offset (+ 32 tag-size))
                      (decf stream-len (+ 32 tag-size)))))))))
        ;; Seek back to the start
        (unless (%drmp3-seek-memory mp3 0 :set)
          (return-from drmp3-init-memory nil))
        (setf (drmp3-stream-length mp3) stream-len
              (drmp3-data-size mp3) stream-len)))
    ;; ID3v2 tags
    (let ((header (%drmp3-read-memory mp3 10)))
      (unless (= (length header) 10)
        (return-from drmp3-init-memory nil))
      (if (equalp (subseq header 0 3) (map 'vector #'char-code "ID3"))
          (let ((tag-size (logior (ash (logand (aref header 6) #x7f) 21) (ash (logand (aref header 7) #x7f) 14)
                                  (ash (logand (aref header 8) #x7f) 7) (logand (aref header 9) #x7f))))
            ;; Account for the footer
            (when (logtest (aref header 5) #x10)
              (incf tag-size 10))
            (unless (%drmp3-seek-memory mp3 tag-size :cur)
              (return-from drmp3-init-memory nil))
            (setf (drmp3-stream-start-offset mp3) (+ (drmp3-stream-start-offset mp3) 10 tag-size)
                  (drmp3-stream-cursor mp3) (drmp3-stream-start-offset mp3)))
          ;; Not an ID3v2 tag. Seek back to the start
          (unless (%drmp3-seek-memory mp3 0 :set)
            (return-from drmp3-init-memory nil))))
    ;; Decode the first frame to confirm that it is indeed a valid MP3 stream. Note that it's possible the first frame
    ;; is actually a Xing/LAME/VBRI header. If this is the case we need to skip over it
    (multiple-value-bind (first-frame-pcm-frame-count first-frame-bytes first-frame-data)
        (%drmp3-decode-next-frame mp3)
      (when (= first-frame-pcm-frame-count 0)
        (return-from drmp3-init-memory nil))   ; Not a valid MP3 stream
      (let* ((data (drmp3-data mp3))
             (f first-frame-data)
             (bs (%make-drmp3-bs data (+ f +drmp3-hdr-size+) 0 (* (- first-frame-bytes +drmp3-hdr-size+) 8)))
             (gr-info (let ((v (make-array 4))) (dotimes (i 4 v) (setf (svref v i) (make-drmp3-gr-info))))))
        (when (%drmp3-hdr-is-crc data f)
          (%drmp3-bs-get-bits bs 16))
        (when (>= (%drmp3-l3-read-side-info bs gr-info data f) 0)
          (block done-xing-info
            (let* ((tag-data-beg (+ f +drmp3-hdr-size+ (floor (drmp3-bs-pos bs) 8)))
                   (tag (- tag-data-beg f)))
              (flet ((remaining () (- first-frame-bytes tag))
                     (b (k) (aref data (+ f tag k))))
                (when (< (remaining) 8) (return-from done-xing-info))   ; Frame too small for a Xing/Info tag
                (let ((is-xing (and (= (b 0) (char-code #\X)) (= (b 1) (char-code #\i)) (= (b 2) (char-code #\n)) (= (b 3) (char-code #\g))))
                      (is-info (and (= (b 0) (char-code #\I)) (= (b 1) (char-code #\n)) (= (b 2) (char-code #\f)) (= (b 3) (char-code #\o)))))
                  (when (or is-xing is-info)
                    (let ((flags (b 7)))
                      (incf tag 8)   ; Skip past the ID and flags
                      (when (logtest flags #x01)   ; FRAMES flag
                        (when (< (remaining) 4) (return-from done-xing-info))
                        (setf detected-mp3-frame-count (logior (ash (b 0) 24) (ash (b 1) 16) (ash (b 2) 8) (b 3)))
                        (incf tag 4))
                      (when (logtest flags #x02)   ; BYTES flag
                        (when (< (remaining) 4) (return-from done-xing-info))
                        (incf tag 4))
                      (when (logtest flags #x04)   ; TOC flag
                        (when (< (remaining) 100) (return-from done-xing-info))
                        (incf tag 100))
                      (when (logtest flags #x08)   ; SCALE flag
                        (when (< (remaining) 4) (return-from done-xing-info))
                        (incf tag 4))
                      ;; At this point we're done with the Xing/Info header. Now we can look at the LAME data
                      (when (/= (b 0) 0)
                        (when (< (remaining) 36) (return-from done-xing-info))
                        (incf tag 21)
                        (let ((delay (+ (logior (ash (b 0) 4) (ash (b 1) -4)) (+ 528 1)))
                              (padding (- (logior (ash (logand (b 1) #xf) 8) (b 2)) (+ 528 1))))
                          (when (< padding 0) (setf padding 0))
                          (setf (drmp3-delay-in-pcm-frames mp3) delay
                                (drmp3-padding-in-pcm-frames mp3) padding)))
                      (if is-xing (setf (drmp3-is-vbr mp3) t) (setf (drmp3-is-cbr mp3) t))
                      ;; Since this was identified as a tag, we don't want to treat it as audio
                      (setf (drmp3-pcm-frames-remaining-in-mp3-frame mp3) 0)
                      ;; The start offset needs to be moved to the end of this frame
                      (incf (drmp3-stream-start-offset mp3) first-frame-bytes)
                      (setf (drmp3-stream-cursor mp3) (drmp3-stream-start-offset mp3))
                      ;; The internal decoder needs to be reset to clear out any state
                      (drmp3dec-init (drmp3-decoder mp3)))))))))
        (when (/= detected-mp3-frame-count #xffffffff)
          (setf (drmp3-total-pcm-frame-count mp3) (* detected-mp3-frame-count first-frame-pcm-frame-count)))
        (setf (drmp3-channels mp3) (drmp3-mp3-frame-channels mp3)
              (drmp3-sample-rate mp3) (drmp3-mp3-frame-sample-rate mp3))
        mp3))))

(defun drmp3-init-file (file-name)
  "Initialize an MP3 decoder from a file, the whole file is loaded to memory"
  (multiple-value-bind (data size) (load-file-data file-name)
    (when data (drmp3-init-memory data size))))

(defun drmp3-uninit (mp3)
  (setf (drmp3-data mp3) nil))

;; drmp3_read_pcm_frames_raw(), OUT is a (signed-byte 16) array or NIL
(defun %drmp3-read-pcm-frames-raw (mp3 frames-to-read out out-pos)
  (let ((total-frames-read 0)
        (total (drmp3-total-pcm-frame-count mp3))
        (padding (drmp3-padding-in-pcm-frames mp3)))
    (loop while (> frames-to-read 0)
          do ;; Skip frames if necessary
             (when (< (drmp3-current-pcm-frame mp3) (drmp3-delay-in-pcm-frames mp3))
               (let ((frames-to-skip (min (drmp3-pcm-frames-remaining-in-mp3-frame mp3)
                                          (- (drmp3-delay-in-pcm-frames mp3) (drmp3-current-pcm-frame mp3)))))
                 (incf (drmp3-current-pcm-frame mp3) frames-to-skip)
                 (incf (drmp3-pcm-frames-consumed-in-mp3-frame mp3) frames-to-skip)
                 (decf (drmp3-pcm-frames-remaining-in-mp3-frame mp3) frames-to-skip)))
             (let ((frames-to-consume (min (drmp3-pcm-frames-remaining-in-mp3-frame mp3) frames-to-read)))
               ;; Clamp the number of frames to read to the padding
               (when (and (/= total +drmp3-uint64-max+) (> total padding))
                 (if (< (drmp3-current-pcm-frame mp3) (- total padding))
                     (setf frames-to-consume (min frames-to-consume (- (- total padding) (drmp3-current-pcm-frame mp3))))
                     (return)))   ; We're into the padding. Abort
               (when out
                 (let ((src (* (drmp3-pcm-frames-consumed-in-mp3-frame mp3) (drmp3-mp3-frame-channels mp3)))
                       (n (* frames-to-consume (drmp3-channels mp3))))
                   (replace out (drmp3-pcm-frames mp3) :start1 (+ out-pos (* total-frames-read (drmp3-channels mp3)))
                                                       :start2 src :end2 (+ src n))))
               (incf (drmp3-current-pcm-frame mp3) frames-to-consume)
               (incf (drmp3-pcm-frames-consumed-in-mp3-frame mp3) frames-to-consume)
               (decf (drmp3-pcm-frames-remaining-in-mp3-frame mp3) frames-to-consume)
               (incf total-frames-read frames-to-consume)
               (decf frames-to-read frames-to-consume)
               (when (= frames-to-read 0) (return))
               ;; If the cursor is already at the padding we need to abort
               (when (and (/= total +drmp3-uint64-max+) (> total padding)
                          (>= (drmp3-current-pcm-frame mp3) (- total padding)))
                 (return))
               ;; At this point we have exhausted our in-memory buffer so we need to re-fill
               (when (= (%drmp3-decode-next-frame mp3) 0)
                 (return))))
    total-frames-read))

(defun drmp3-read-pcm-frames-s16 (mp3 frames-to-read out &optional (out-pos 0))
  (%drmp3-read-pcm-frames-raw mp3 frames-to-read out out-pos))

(defun drmp3-read-pcm-frames-f32 (mp3 frames-to-read out &optional (out-pos 0))
  "Read frames converted from s16 to f32 into OUT (a single-float array)"
  (let* ((temp (make-array (* 1152 2) :element-type '(signed-byte 16) :initial-element 0))
         (total-pcm-frames-read 0))
    (loop while (< total-pcm-frames-read frames-to-read)
          do (let* ((frames-to-read-now (min (floor (* 1152 2) (drmp3-channels mp3))
                                             (- frames-to-read total-pcm-frames-read)))
                    (frames-just-read (%drmp3-read-pcm-frames-raw mp3 frames-to-read-now temp 0)))
               (when (= frames-just-read 0) (return))
               ;; drmp3_s16_to_f32()
               (dotimes (i (* frames-just-read (drmp3-channels mp3)))
                 (setf (aref out (+ out-pos (* total-pcm-frames-read (drmp3-channels mp3)) i))
                       (* (float (aref temp i) 1f0) 0.000030517578125f0)))
               (incf total-pcm-frames-read frames-just-read)))
    total-pcm-frames-read))

(defun %drmp3-reset (mp3)
  (setf (drmp3-pcm-frames-consumed-in-mp3-frame mp3) 0
        (drmp3-pcm-frames-remaining-in-mp3-frame mp3) 0
        (drmp3-current-pcm-frame mp3) 0)
  (drmp3dec-init (drmp3-decoder mp3)))

(defun drmp3-seek-to-start-of-stream (mp3)
  (unless (%drmp3-seek-memory mp3 (drmp3-stream-start-offset mp3) :set)
    (return-from drmp3-seek-to-start-of-stream nil))
  ;; Clear any cached data
  (%drmp3-reset mp3)
  t)

(defun drmp3-seek-to-pcm-frame (mp3 frame-index)
  (cond ((= frame-index 0) (drmp3-seek-to-start-of-stream mp3))
        ((= frame-index (drmp3-current-pcm-frame mp3)) t)
        (t
         ;; If we're moving foward we just read from where we're at. Otherwise we need to move back to the start of
         ;; the stream and read from the beginning
         (when (< frame-index (drmp3-current-pcm-frame mp3))
           (unless (drmp3-seek-to-start-of-stream mp3) (return-from drmp3-seek-to-pcm-frame nil)))
         (let ((offset (- frame-index (drmp3-current-pcm-frame mp3))))
           (= (%drmp3-read-pcm-frames-raw mp3 offset nil 0) offset)))))

(defun drmp3-get-mp3-and-pcm-frame-count (mp3)
  "Returns (values ok mp3-frame-count pcm-frame-count)"
  (let ((current-pcm-frame (drmp3-current-pcm-frame mp3))
        (total-pcm-frame-count 0)
        (total-mp3-frame-count 0))
    (unless (drmp3-seek-to-start-of-stream mp3) (return-from drmp3-get-mp3-and-pcm-frame-count nil))
    (loop
      (let ((pcm-frames-in-current-mp3-frame (%drmp3-decode-next-frame-ex mp3 nil)))
        (when (= pcm-frames-in-current-mp3-frame 0) (return))
        (incf total-pcm-frame-count pcm-frames-in-current-mp3-frame)
        (incf total-mp3-frame-count)))
    ;; Finally, we need to seek back to where we were
    (unless (drmp3-seek-to-start-of-stream mp3) (return-from drmp3-get-mp3-and-pcm-frame-count nil))
    (unless (drmp3-seek-to-pcm-frame mp3 current-pcm-frame) (return-from drmp3-get-mp3-and-pcm-frame-count nil))
    (values t total-mp3-frame-count total-pcm-frame-count)))

(defun drmp3-get-pcm-frame-count (mp3)
  (if (/= (drmp3-total-pcm-frame-count mp3) +drmp3-uint64-max+)
      (let ((total (drmp3-total-pcm-frame-count mp3)))
        (when (>= total (drmp3-delay-in-pcm-frames mp3)) (decf total (drmp3-delay-in-pcm-frames mp3)))
        (when (>= total (drmp3-padding-in-pcm-frames mp3)) (decf total (drmp3-padding-in-pcm-frames mp3)))
        total)
      (multiple-value-bind (ok mp3-frames pcm-frames) (drmp3-get-mp3-and-pcm-frame-count mp3)
        (declare (ignore mp3-frames))
        (if ok pcm-frames 0))))

(defun drmp3-open-memory-and-read-pcm-frames-f32 (data data-size)
  "Decode a whole MP3 stream, returns (values samples channels sample-rate total-frame-count) or NIL"
  (let ((mp3 (drmp3-init-memory data data-size)))
    (when mp3
      (let ((chunks '())
            (total-frames-read 0)
            (temp (make-array (* 1152 2) :element-type 'single-float :initial-element 0f0)))
        (loop
          (let* ((frames-to-read-right-now (floor (* 1152 2) (drmp3-channels mp3)))
                 (frames-just-read (drmp3-read-pcm-frames-f32 mp3 frames-to-read-right-now temp 0)))
            (when (= frames-just-read 0) (return))
            (push (subseq temp 0 (* frames-just-read (drmp3-channels mp3))) chunks)
            (incf total-frames-read frames-just-read)
            ;; If the number of frames we asked for is less that what we actually read it means we've reached the end
            (when (/= frames-just-read frames-to-read-right-now) (return))))
        (let ((channels (drmp3-channels mp3))
              (sample-rate (drmp3-sample-rate mp3)))
          (drmp3-uninit mp3)
          (when chunks
            (values (apply #'concatenate '(simple-array single-float (*)) (nreverse chunks))
                    channels sample-rate total-frames-read)))))))
