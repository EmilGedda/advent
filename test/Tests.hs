module Tests where

import qualified Solutions.Y2020 as Y20
import qualified Solutions.Y2021 as Y21
import Types (Answers(..), answer)

answers :: [Answers]
answers = [
        Answers 2020 [
            answer Y20.day01 970816         96047280,
            answer Y20.day02 666            670,
            answer Y20.day03 223            3517401300,
            answer Y20.day04 235            194,
            answer Y20.day05 998            676,
            answer Y20.day06 6387           3039,
            answer Y20.day07 242            176035,
            answer Y20.day08 1200           1023,
            answer Y20.day09 1639024365     219202240,
            answer Y20.day10 1980           4628074479616,
            answer Y20.day11 2346           2111,
            answer Y20.day12 2879           178986,
            answer Y20.day13 119            1106724616194525,
            answer Y20.day14 17028179706934 3683236147222,
            answer Y20.day15 475            11261,
            answer Y20.day16 21956          3709435214239,
            answer Y20.day17 362            1980,
            answer Y20.day18 6923486965641  70722650566361,
            answer Y20.day19 111            343,
            answer Y20.day20 18449208814679 1559,
            answer Y20.day21 2020           "bcdgf,xhrdsl,vndrb,dhbxtb,lbnmsr,scxxn,bvcrrfbr,xcgtv",
            answer Y20.day22 32489          35676,
            answer Y20.day23 28793654       359206768694,
            answer Y20.day24 289            3551,
            answer Y20.day25 3803729        Nothing
        ],
        Answers 2021 [
            answer Y21.day01 1121    1065,
            answer Y21.day02 1893605 2120734350,
            answer Y21.day03 4001724 587895,
            answer Y21.day04 41503   3178,
            answer Y21.day05 5576    18144,
            answer Y21.day06 379114  1702631502303,
            answer Y21.day07 352254  99053143,
            answer Y21.day08 383     998900
        ]
    ]
