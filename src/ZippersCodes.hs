module ZippersCodes where

import DataTypeAndTypeclasses (List' (..), Tree (EmptyTree, Node), treeInsert)

-- There are many different kinds of trees.
freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' EmptyTree EmptyTree)
            (Node 'T' EmptyTree EmptyTree)
        )
        ( Node
            'Y'
            (Node 'S' EmptyTree EmptyTree)
            (Node 'A' EmptyTree EmptyTree)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' EmptyTree EmptyTree)
            (Node 'R' EmptyTree EmptyTree)
        )
        ( Node
            'A'
            (Node 'A' EmptyTree EmptyTree)
            (Node 'C' EmptyTree EmptyTree)
        )
    )

-- want to change the 'W' to 'P'
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)

type Directions = [Direction]

-- want to change the 'W' to 'P' with Directions
changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L : ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R : ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree p -> p
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

-- Zipper: a pair that contains a focused part of a data structure and its surroundings
-- Breadcrumbs
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Zipper a = (Tree a, Breadcrumbs a)

type Breadcrumbs a = [Crumb a]

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

-- return `focused Tree` and other infomation except the `focused Tree`, which is value of parent Tree and the Tree on opposite side. So we can reconstruct parent Tree again with those infomation.
goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, RightCrumb x l : bs)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x r : bs) = (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = (Node x l t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (EmptyTree, bs) = (EmptyTree, bs)

-- let farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
-- let newFocus = farLeft -: attach (Node 'Z' EmptyTree EmptyTree)
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- First list is the currently focused list, and second list is the list of breadcrumbs.
type ListZipper a = ([a], [a])

-- goTop $ goBottom (xs,[])
goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b : bs) = (b : xs, bs)

goBottom :: ListZipper a -> ListZipper a
goBottom ([x], bs) = ([x], bs)
goBottom z = goBottom (goForward z)

goTop :: ListZipper a -> ListZipper a
goTop (xs, []) = (xs, [])
goTop z = goTop (goBack z)

-- simple file system

type Name = String

type Data = String

-- file system item
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "jijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code "
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

{-
Note that if the name we're looking for isn't in the folder, the pattern item:rs will try to match on an empty list and we'll get an error. Also, if our current focus isn't a folder at all but a file, we get an error as well and the program crashes.
-}
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item : rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs : bs)

-- Auxilliary function
nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder _ items, bs) = (Folder newName items, bs)
fsRename newName (File _ dat, bs) = (File newName dat, bs)

-- Note that this would crash if we tried to add an item but weren't focusing on a folder, but were focusing on a file instead.
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item : items), bs)