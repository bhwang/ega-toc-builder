{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Lucid
import Lucid.Base (termRawWith)

main :: IO ()
main = do
  ega1 <- readFile "contents/ega-i.md"
  ega2 <- readFile "contents/ega-ii.md"
  ega3a <- readFile "contents/ega-iii-1.md"
  ega3b <- readFile "contents/ega-iii-2.md"
  ega4a <- readFile "contents/ega-iv-1.md"
  ega4b <- readFile "contents/ega-iv-2.md"
  ega4c <- readFile "contents/ega-iv-3.md"
  ega4d <- readFile "contents/ega-iv-4.md"

  renderToFile "output/index.html" $ makePage "EGA Table of Contents" $
    concat $ map sectionFormat
       [ ("EGA I. Le langage des schémas", ega1)
       , ("EGA II. Étude globale élémentaire de quelques classes de morphismes", ega2)
       , ("EGA III. Étude cohomologique des faisceaux cohérents, Première partie", ega3a)
       , ("EGA III. Étude cohomologique des faisceaux cohérents, Seconde partie", ega3b)
       , ("EGA IV. Étude locale des schémas et des morphismes de schémas, Première partie", ega4a)
       , ("EGA IV. Étude locale des schémas et des    morphismes de schémas, Seconde partie", ega4b)
       , ("EGA IV. Étude locale des schémas et des    morphismes de schémas, Troisième partie", ega4c)
       , ("EGA IV. Étude locale des schémas et des    morphismes de schémas, Quatrième partie", ega4d)
       ]

sectionFormat :: (Html (), String) -> [Html ()]
sectionFormat (title, filecontents) = 
  [ with div_ [id_ "volume"] (h2_ $ toHtml title)
  , makeSection $ inputToSections filecontents
  ]

-----------------------------
-- PARSING TO SECTION TYPE --
-----------------------------

-- | Top function, maps a formatted string to a list of Sections (which themselves
-- contain their subesections).
inputToSections :: String -> [Section]
inputToSections s = makeSections $ parsePairs $ takePairs $ filter (not . null) $ lines s

type Title = String
type Page = Int
data Section = Section { getTitle :: Title, getPage :: Page, getSubsections ::  [Section] }
  deriving (Eq, Show)

-- Note: FAILs when applied to lists with an odd number of elts
takePairs :: [a] -> [(a, a)]
takePairs [] = []
takePairs [_] = error "ERROR: List has odd number of elements!"
takePairs xs = (xs !! 0, xs !! 1) : takePairs (drop 2 xs)

type Nesting = Int
-- "Nesting" keeps tracks of the nesting level. e.g. top-level section is has
-- nesting level 1, a subsection of it is has nesting level 2, a subsection of
-- this subsection is nesting level 3, and so on.

parsePairs :: [(String, String)] -> [(Title, Nesting, Int)]
parsePairs [] = []
parsePairs (x:xs) = [(title, nestingLevel, page)] ++ parsePairs xs
  where (titleString, pageString) = x
        page = (read pageString) :: Int
        nestingLevel = length (takeWhile (== '#') titleString)
        title = drop (nestingLevel + 1) titleString

makeSections :: [(Title, Nesting, Int)] -> [Section]
makeSections [] = []
makeSections (x:xs) = (Section topTitle topPage (makeSections nextBatch)):      (makeSections $ drop (length nextBatch) xs)
  where (topTitle, topLevel, topPage) = x
        nextBatch = takeWhile (\(_, level, _) -> level > topLevel) xs


--------------------------------
-- RENDERING SECTIONS TO HTML --
--------------------------------
-- Requires the `lucid` library

sectionsToHtml :: [Section] -> Html ()
sectionsToHtml = mconcat . map sectionToHtml

sectionToHtml :: Section -> Html ()
sectionToHtml section =  li_ $ do
  a_ $ with span_ [class_ "title"] (toHtml (getTitle section))
       <> " "
       <> (with span_
                  [class_ "page"]
                  (toHtml $ show (getPage section))
          )

  -- Construct subsections, if available.
  let subsections = getSubsections section
  if null subsections
  then return () -- do nothing
  else ol_ (sectionsToHtml $ getSubsections section)

makeSection :: [Section] -> Html ()
makeSection sections = ol_ $ do
    sectionsToHtml sections

-- Main method to construct the webpage
makePage :: String -> [Html ()] -> Html ()
makePage title parts = html_ $ do
    head_ $ do
        title_ (toHtml title)
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        link_ [rel_ "stylesheet", href_ "style.css"]
        mathJaxScripts

    body_ $ with div_ [id_ "cage"] $ do
        with div_ [id_ "header"] (h1_ $ toHtml title)

        mapM_ id parts


mathJaxScripts :: Html ()
mathJaxScripts = do
  termRawWith "script"  [src_ "https://polyfill.io/v3/polyfill.min.js?          features=es6"] ""
  termRawWith "script" [async_ "", src_ "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js"] ""
