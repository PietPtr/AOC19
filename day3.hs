-- build list of coordinates occupied by wire 1 and wire 2
-- find all coordinates that are the same
-- select the one closest to the port (0,0)

import Debug.Trace
import Data.List (sort)
import Data.Ord (comparing)

type Coord = (Int, Int)

type Path = [Part]
data Part =
      U Int
    | D Int
    | L Int
    | R Int
    deriving (Show, Eq)

wire :: Int -> Path
-- wire 1 = [R 8, U 5, L 5, D 3]
-- wire 2 = [U 7, R 6, D 4, L 4]
-- wire 1 = [R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72]
-- wire 2 = [U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83]
wire 1 = [R 1006, D 541, R 261, U 378, L 530, U 165, L 175, U 143, R 162, D 504, R 985, U 33, R 544, D 168, L 498, D 549, R 88, D 243, L 36, U 944, R 261, D 91, L 957, D 579, L 224, D 732, R 312, U 378, R 82, D 200, L 510, U 747, R 588, U 667, L 495, D 147, L 100, U 482, R 896, D 711, L 513, U 44, L 685, U 547, L 132, D 23, R 139, U 786, L 908, U 912, R 531, U 564, L 970, D 562, R 422, U 919, R 108, D 275, R 431, U 697, L 85, D 762, L 25, D 633, R 878, U 566, L 550, D 288, L 29, D 788, R 930, U 619, L 612, U 228, R 25, D 133, R 219, U 367, L 889, U 735, L 994, U 513, R 34, D 429, L 750, U 83, R 204, U 68, R 769, D 833, L 545, D 621, L 747, U 714, R 655, U 112, L 629, D 353, L 450, D 588, R 775, U 493, L 252, D 486, L 405, D 350, R 970, D 73, L 750, D 731, L 962, D 242, R 947, D 348, L 252, D 392, L 94, U 970, R 616, U 505, L 782, D 375, R 849, U 971, R 57, D 25, R 68, U 174, L 670, U 735, R 66, D 994, R 868, U 285, L 866, U 433, L 169, D 575, L 374, U 169, R 180, D 251, R 671, D 703, R 708, D 60, L 251, D 164, L 106, U 974, R 670, U 760, L 235, U 377, R 318, U 294, L 421, D 904, L 571, U 157, R 428, D 416, L 237, D 850, L 827, U 702, L 134, D 67, R 327, U 976, L 307, D 454, L 646, U 919, L 92, D 523, R 828, D 544, L 557, D 142, L 671, D 862, R 118, U 244, L 667, U 356, L 554, U 969, R 348, D 895, L 735, D 948, R 920, U 470, R 819, D 256, R 169, D 410, R 977, U 487, L 64, U 466, L 574, U 891, R 29, D 767, L 224, D 922, L 782, U 433, L 478, U 582, L 603, U 339, L 658, U 188, L 95, U 766, R 958, U 313, L 881, D 869, L 633, U 551, R 270, U 444, R 399, D 698, L 923, U 213, R 245, D 486, R 34, U 514, R 450, U 739, R 102, U 181, L 826, D 839, L 948, D 11, R 51, U 146, R 415, U 683, R 352, U 387, R 158, D 88, L 576, U 600, R 955, D 22, R 884, D 772, L 576, D 74, L 937, U 832, R 198, D 721, R 393, U 847, R 828, U 975, L 452, U 796, R 950, U 568, R 117, U 114, L 983, U 514, R 564, U 569, L 141, D 464, R 463, U 635, L 772, U 634, R 614, D 160, R 388, D 550, L 933, D 832, R 94, D 855, L 18, U 241, L 805, U 517, R 384, D 464, L 271, U 788, R 718, U 495, R 103]
wire 2 = [L 1000, D 65, L 329, D 227, R 798, U 36, R 263, D 232, R 771, D 768, R 223, D 898, L 637, U 402, L 867, U 694, R 362, U 199, L 769, U 956, L 180, U 123, L 495, U 660, L 861, D 652, R 222, D 166, R 47, D 766, R 709, U 859, L 639, U 841, L 407, D 392, R 503, D 596, R 614, D 448, L 340, D 562, L 913, U 942, L 426, D 84, R 385, U 202, R 676, U 379, L 231, D 124, L 568, D 134, L 271, D 777, R 765, U 979, R 678, D 478, R 307, D 568, L 318, D 705, R 787, U 322, R 233, D 423, L 617, U 955, R 32, U 989, R 356, U 922, R 444, U 443, R 136, U 140, L 298, U 348, L 121, U 332, R 285, D 302, L 844, D 234, L 468, U 395, R 20, D 245, L 583, U 173, L 928, U 598, L 383, D 188, L 945, D 382, L 929, D 181, L 650, U 394, L 938, U 805, L 680, U 676, R 136, U 925, L 899, U 990, R 661, D 621, R 612, D 587, R 609, U 560, R 650, D 416, L 285, D 152, R 906, U 47, L 721, D 206, L 602, U 258, R 667, U 443, L 291, D 375, L 977, D 148, R 394, U 758, L 43, D 953, R 143, D 60, R 851, D 887, R 718, D 505, R 407, D 321, R 140, D 675, L 42, U 235, L 626, D 673, L 271, D 398, L 190, U 30, L 225, D 612, R 896, U 757, L 340, D 280, L 742, U 188, L 372, D 7, R 677, U 248, R 694, U 581, L 220, U 372, R 497, U 89, R 952, D 221, L 71, D 962, L 992, U 420, R 741, U 96, R 625, U 794, L 602, U 229, R 635, D 585, R 119, U 501, R 640, D 283, L 963, U 385, L 967, D 503, L 453, D 578, L 465, D 318, L 968, U 979, L 650, D 894, L 210, U 855, R 298, D 66, R 378, D 223, L 475, D 950, L 417, D 276, L 494, D 690, R 516, D 352, L 603, U 211, R 171, U 553, L 437, U 865, L 378, D 223, R 814, D 779, L 780, D 738, R 920, D 462, L 230, U 574, L 880, D 252, R 710, D 476, L 184, U 635, R 453, U 115, R 96, U 169, R 995, D 523, R 562, D 480, L 791, U 865, R 568, D 149, L 539, U 610, R 107, D 604, R 95, D 982, R 360, U 141, L 567, D 555, R 481, U 716, R 753, D 576, R 54, D 343, R 663, U 676, R 907, D 202, R 230, U 827, L 583, U 937, R 818, D 579, R 502, D 713, R 61, U 402, L 527, D 955, R 117, U 214, R 580, U 636, R 721, U 55, L 899, U 667, R 595, U 790, L 384, U 416, L 375, D 1, L 653, U 611, L 187, D 256, L 931]

occupies' :: Part -> Coord -> [Coord] -> [Coord]
occupies' (U 0) _ coords = coords
occupies' (D 0) _ coords = coords
occupies' (L 0) _ coords = coords
occupies' (R 0) _ coords = coords
occupies' (U a) (x, y) coords = (x, y-1) : (occupies' (U (a-1)) (x, y-1) coords)
occupies' (D a) (x, y) coords = (x, y+1) : (occupies' (D (a-1)) (x, y+1) coords)
occupies' (L a) (x, y) coords = (x-1, y) : (occupies' (L (a-1)) (x-1, y) coords)
occupies' (R a) (x, y) coords = (x+1, y) : (occupies' (R (a-1)) (x+1, y) coords)

occupies part from = occupies' part from []

to :: Part -> Coord -> Coord
to (U a) (x, y) = (x, y - a)
to (D a) (x, y) = (x, y + a)
to (L a) (x, y) = (x - a, y)
to (R a) (x, y) = (x + a, y)

occupances :: Path -> Coord -> [Coord]
occupances [] _ = []
occupances (part:path) from = (occupies part from) ++ (occupances path (to part from))

intersect' :: (Eq a) => [a] -> [a] -> [a] -> [a]
intersect' [] ys result = result
intersect' (x:xs) ys result
    | x `elem` ys = intersect' xs ys (x:result)
    | otherwise = intersect' xs ys result

intersect xs ys = intersect' xs ys []

crossings = intersect (occupances (wire 1) (0, 0)) (occupances (wire 2) (0, 0))

dist :: (Int, Int) -> Int
dist (x, y) = (abs x) + (abs y)

answer = sort $ map dist crossings
