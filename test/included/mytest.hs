-- ~/~ begin <<doc/de/results/03_Implementation.md#src/Align/Data.hs>>[init]
module Align.Data where

-- ~/~ begin <<doc/de/results/03_Implementation.md#align-imports>>[init]
import Data.Array (Array, (!), listArray, elems)
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#align-imports>>[1]
import Data.Matrix (Matrix, matrix, nrows, ncols, getElem, setElem)
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#align-imports>>[2]
import Data.List (intersperse)
-- ~/~ end

-- ~/~ begin <<doc/de/results/03_Implementation.md#types>>[init]
-- DATA TYPES

-- ~/~ begin <<doc/de/results/03_Implementation.md#type-cost>>[init]
-- | Record type for costs.
data Cost = Cost {w_match :: Int, w_miss :: Int, w_gap :: Int} deriving (Eq, Show)
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#type-seq>>[init]
-- | We model sequences as plain strings, i.e., lists of chars.
type Seq = String

-- | Alignment characters consist of either symbols or gaps.
data AlnChar = Symbol Char | Gap deriving (Eq)

-- | Alignments are list of AlnChar tuples.
type Aln = [(AlnChar, AlnChar)]
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#type-seq-arr>>[init]
-- | We use arrays for constant time access.
type SeqArr = Array Int Char

-- | Create sequence arrays from sequence strings.
-- Arrays allow constant time lookups.
-- Indexing is shifted by one, so we index from 2 through m+1.
mkArr :: Seq -> SeqArr

mkArr xs = let m = length xs
           in listArray (2, m+1) xs
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#type-aln-info>>[init]
-- | Record with key data of the alignment problem.
data AlnInfo = AlnInfo
  { g_max   :: Int
  , weights :: Cost
  , seqA    :: SeqArr
  , seqB    :: SeqArr
  } deriving (Eq, Show)

-- | Compute sequence lengths for an AlnInfo record.
seqLengths :: AlnInfo -> (Int, Int)
seqLengths AlnInfo {seqA = s1, seqB = s2} = (length s1, length s2)

-- | Compute gap numbers for an AlnInfo record.
gapCounts :: AlnInfo -> (Int, Int)
gapCounts info@(AlnInfo {g_max = g})
  = let
      (m, n) = seqLengths info
      l = max m n
    in (g + (l - m), g + (l - n))

-- | Convenience constructor to create AlnInfo records from loose weights and lists.
mkInfo :: Int -> Int -> Int -> Int -> Seq -> Seq -> AlnInfo
mkInfo g_max w_match w_miss w_gap seqA seqB
  = AlnInfo g_max (Cost w_match w_miss w_gap) (mkArr seqA) (mkArr seqB)
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#type-aln-result>>[init]
data AlnResult = AlnResult
  { alnInfo  :: AlnInfo
  , nwMat    :: NWMatrix
  , optAlns  :: [Aln]
  , optScore :: Int
  } deriving Eq
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#type-mat-parts>>[init]
-- | Matrix indices.
type MatIdx = (Int, Int)

-- | Path through a matrix.
type Path = [MatIdx]

-- | Steps are `MatIdx` tuples of the form `(origin, destination)`.
type Step = (MatIdx, MatIdx)

-- | Datatype to denote step directions.
data StepDirection = Diagonal | Horizontal | Vertical deriving Eq

-- | Calculate a StepDirection from a Step.
fromStep :: Step -> StepDirection
fromStep (orig@(g, h), dest@(i, j))
  | i == g+1 && j == h+1 = Diagonal
  | i == g   && j == h+1 = Horizontal
  | i == g+1 && j == h   = Vertical
  | otherwise = error "illegal step"
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#type-nw-matrix>>[init]
-- | What goes into the matrix?
-- If a cell is defined, we have a value f_ij,
-- and a list of one or more precursors.
type CellValue = Maybe (Int, [StepDirection])

-- | A Needleman-Wunsch matrix.
type NWMatrix = Matrix CellValue
-- ~/~ end
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#helpers>>[init]
-- HELPER FUNCTIONS

-- helpers for matrix computation
-- ~/~ begin <<doc/de/results/03_Implementation.md#dist-diag>>[init]
-- | Helper function to compute whether an alignment introduces too many gaps.
-- True if distance |i-j| from main diagonal is lesser or equal to allowed gaps.
distDiag :: MatIdx -> Int -> Bool
distDiag (i, j) gaps = abs (i-j) <= gaps

-- | Whether an index is on the left side of the main diagonal (or on it).
onLeft :: MatIdx -> Bool
onLeft (i, j) = i-j >= 0

-- Helpers for our cell consideration criteria I, II, and III.
inHRange :: MatIdx -> Int -> Bool
inHRange (i, j) g1 = distDiag (i, j-1) g1

inVRange :: MatIdx -> Int -> Bool
inVRange (i, j) g2 = distDiag (i-1, j) g2

inDRange :: MatIdx -> Int -> Int -> Bool
inDRange cell g1 g2
  | onLeft cell = distDiag cell g1
  | otherwise   = distDiag cell g2
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#candidates>>[init]
-- | Find potential candidate cells, from which f_ij could be derived.
candidates :: MatIdx -> Int -> Int -> [MatIdx]
candidates (i, j) g1 g2
  | _I && _II && _III = [d, h, v]
  | _I && _II         = [d, h]
  | _I && _III        = [d, v]
  | _I                = [d]
  | otherwise         = []
    where
      _I   = inDRange (i, j) g1 g2
      _II  = inHRange (i, j) g1
      _III = inVRange (i, j) g2

      d = (i-1, j-1)
      h = (i  , j-1)
      v = (i-1, j  )
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#next-cell>>[init]
-- | Compute the index of the next cell to calculate.
nextCell :: AlnInfo -> MatIdx -> Maybe MatIdx
nextCell info cell@(i, j)
  | incCol = Just nxcol
  | incRow = Just nxrow
  | otherwise = Nothing
    where
      (m, n) = seqLengths info
      (g1, g2) = gapCounts info
      nxcol = (i  , j+1)
      nxrow = (i+1, max 2 (i+1 - g2))
      incCol = j < n+1 && inDRange nxcol g1 g2
      incRow = i < m+1 && inDRange nxrow g1 g2
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#weight>>[init]
-- | Calculate the weight of a substitution.
substWeight :: (Eq a) => Cost -> (a, a) -> Int
substWeight (Cost {w_match = match, w_miss = miss}) (s1, s2)
  | s1 == s2  = match
  | otherwise = miss

-- | Calculate the weight of a step.
stepWeight :: Cost -> SeqArr -> SeqArr -> Step -> Int
stepWeight cost@(Cost {w_gap = gap}) seqA seqB (orig@(g, h), dest@(i, j))
  | isSubst   = substWeight cost (seqA ! i, seqB ! j)
  | otherwise = gap
    where
      isSubst = i == g + 1 && j == h + 1

-- | Score a candidate step.
stepScore :: AlnInfo -> Step -> NWMatrix -> (Maybe Int, StepDirection)
stepScore info@(AlnInfo {seqA = s1, seqB = s2}) (candidate@(g, h), cell) mat
  = (score, dir)
    where
      val = getElem g h mat
      w = stepWeight (weights info) s1 s2 (candidate, cell)
      score = fmap ((+w).fst) val
      dir = fromStep (candidate, cell)
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#max-val>>[init]
-- | Find the directions of maximum weight.
-- This uses two passes through a list with length <= 3 -> runs in O(1)
maxValDirs :: [(Maybe Int, StepDirection)] -> CellValue
maxValDirs sc =
  let
    m  = max' Nothing sc
    ds = dirs m sc
  in m `seq` ds `seq` result m ds
  where
    -- find the highest candidate score
    max' :: Maybe Int -> [(Maybe Int, StepDirection)] -> Maybe Int
    max' accum [] = accum
    max' accum ((s,_):sc)
      | s > accum = max' s sc
      | otherwise = max' accum sc

    -- collect directions with that score
    dirs :: Maybe Int -> [(Maybe Int, StepDirection)] -> [StepDirection]
    dirs _ [] = []
    dirs m ((s, d):sc)
      | s == m    = d : rest
      | otherwise = rest
        where
          rest = dirs m sc

    -- "lift" the list of directions into the Maybe
    result :: Maybe Int -> [StepDirection] -> CellValue
    result Nothing _   = Nothing
    result (Just v) ds = Just (v, ds)
-- ~/~ end

-- ~/~ begin <<doc/de/results/03_Implementation.md#init-mat>>[init]
initFillFunc :: Cost -> Int -> Int -> MatIdx -> CellValue
initFillFunc (Cost {w_gap = gap}) g1 g2 cell@(i, j)
  | valid && i == 1 && j == 1 = Just (          0, [])
  | valid && i == 1           = Just ((j-1) * gap, [Horizontal])
  | valid && j == 1           = Just ((i-1) * gap, [Vertical])
  | otherwise = Nothing
    where
      valid = inDRange cell g1 g2

initMat :: AlnInfo -> NWMatrix
initMat info@(AlnInfo {weights = cost})
  -- | trace ("initMat " ++ show (m+1, n+1)) False = undefined
  | otherwise = matrix (m+1) (n+1) $ initFillFunc cost g1 g2
    where
      (m, n) = seqLengths info
      (g1, g2) = gapCounts info
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#fill-cell>>[init]
fillCell :: AlnInfo -> MatIdx -> NWMatrix -> NWMatrix
fillCell info@(AlnInfo {seqA = s1, seqB = s2}) cell mat
  -- we use `seq` to force strict evaluation of best, so mat doesn't stay in memory
  = best `seq` setElem best cell mat
    where
      (g1, g2) = gapCounts info
      idxs = candidates cell g1 g2
      scoredCandidates :: [(Maybe Int, StepDirection)]
      scoredCandidates = map (\candidate -> stepScore info (candidate, cell) mat) idxs
      best = maxValDirs scoredCandidates
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#fill-from>>[init]
-- | Given AlnInfo, a matrix index and an NWMatrix with the
-- previous indices filled, fill the rest of the matrix,
-- beginning at the given index.
fillFrom :: AlnInfo -> MatIdx -> NWMatrix -> NWMatrix
fillFrom info cell mat
  -- we use `seq` to force strict evaluation, so stuff doesn't overflow memory
  = next `seq` m `seq`
  case next of
    -- last cell  -> fill cell and stop
    Nothing -> m
    -- more cells -> fill cell and continue
    Just next  -> fillFrom info next m
  where
    next = nextCell info cell
    m = fillCell info cell mat
-- ~/~ end

-- helpers for backtracking
-- ~/~ begin <<doc/de/results/03_Implementation.md#origs>>[init]
-- | Calculate the origin of a StepDirection from a particular position.
getOrig :: MatIdx -> StepDirection -> MatIdx
getOrig dest@(i, j) Diagonal   = (i-1, j-1)
getOrig dest@(i, j) Horizontal = (i  , j-1)
getOrig dest@(i, j) Vertical   = (i-1, j  )

origs :: MatIdx -> CellValue -> [MatIdx]
origs cell@(i, j) elem =
  case elem of
    Nothing        -> []
    Just (_, dirs) -> map (getOrig cell) dirs
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#find-rev-paths>>[init]
-- | Helper for backtracking, that determines the
-- (reverse) matrix paths for a given index.
findRevPaths :: NWMatrix -> MatIdx -> [Path]
findRevPaths _ (1, 1) = [[(1, 1)]]
findRevPaths mat cell@(i, j) = (prepend.collect.continue) cellOrigs
  where
    elem :: CellValue
    elem = getElem i j mat

    cellOrigs = origs cell elem
    continue = map (findRevPaths mat)
    collect = concat
    prepend = map (cell:)
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#convert-path>>[init]
-- | Compute the alignment of two sequences from a matrix path.
convertPath :: SeqArr -> SeqArr -> Path -> Aln
convertPath _ _ [] = []
convertPath _ _ [p] = []
convertPath s1 s2 (p@(g, h):p'@(i, j):ps) =
  case dir of
    Diagonal   -> (sym1, sym2) : rest
    Horizontal -> (Gap , sym2) : rest
    Vertical   -> (sym1, Gap ) : rest
  where
    dir  = fromStep (p, p')
    sym1 = Symbol (s1 ! i)
    sym2 = Symbol (s2 ! j)
    rest = convertPath s1 s2 (p':ps)
-- ~/~ end
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#computation>>[init]
-- COMPUTATIONS

-- ~/~ begin <<doc/de/results/03_Implementation.md#fill>>[init]
fill :: AlnInfo -> NWMatrix
fill info = let mat = initMat info
            in mat `seq` fillFrom info (2, 2) mat
            where counts = gapCounts info
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#backtrack>>[init]
-- | Given a filled in NWMatrix for two sequences,
-- determine the optimal alignments.
backtrack :: NWMatrix -> SeqArr -> SeqArr -> [Aln]
backtrack mat s1 s2 = map (convertPath s1 s2) paths
  where
    revpaths = findRevPaths mat (nrows mat, ncols mat)
    paths = map reverse revpaths
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#align>>[init]
align :: AlnInfo -> AlnResult
align info@(AlnInfo {seqA = s1, seqB = s2}) =
  case maybeScore of
    Just (score, _)  -> AlnResult info mat alns score
    Nothing -> error "global alignment is undefined"
  where
    mat = fill info
    alns = backtrack mat s1 s2
    maybeScore = getElem (nrows mat) (ncols mat) mat
-- ~/~ end
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#representations>>[init]
-- ~/~ begin <<doc/de/results/03_Implementation.md#show-aln-char>>[init]
toChar :: AlnChar -> Char
toChar Gap        = '-'
toChar (Symbol c) = c

instance Show (AlnChar) where
  show = show.toChar
  showList xs ys = showList (map toChar xs) ys
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#show-steps>>[init]
-- | Use arrow symbols to display step directions.
instance Show StepDirection where
  show Diagonal = "↖"
  show Horizontal = "←"
  show Vertical = "↑"
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#show-aln-info>>[init]
showInfo info@(AlnInfo {g_max = g_max', weights = cost, seqA = s1, seqB = s2})
    =  "  g_max   = " ++ show g_max'          ++ "\n"
    ++ "  w_match = " ++ (show.w_match) cost  ++ "\n"
    ++ "  w_miss  = " ++ (show.w_miss) cost   ++ "\n"
    ++ "  w_gap   = " ++ (show.w_gap) cost    ++ "\n"
    ++ "  seqA    = " ++ showSeq s1           ++ "\n"
    ++ "  seqB    = " ++ showSeq s2           ++ "\n"
    where
      showSeq = show.elems
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#show-aln-result>>[init]
-- | Exclude the nwMat field from the show representation.
instance Show AlnResult where
  show (AlnResult {alnInfo = info, optAlns = alns, optScore = score})
    = "AlnResult { "
    ++   "alnInfo = "  ++ show info
    ++ ", optAlns = "  ++ show alns
    ++ ", optScore = " ++ show score
    ++ " }"

showResult :: AlnResult -> String
showResult (AlnResult {alnInfo = info, nwMat = mat, optAlns = alns, optScore = score})
    = "Given a pairwise alignment problem with the following key data:\n\n"
    ++ showInfo info ++ "\n\n"
    ++ "The problem is optimally solved by the following "
    ++ (show.length) alns ++ " global alignment(s)"
    ++ ", with score " ++ show score ++ ":\n\n"
    ++ showAlns alns ++ "\n\n"
    -- ++ "Results are derived from the following NW-matrix:\n\n" ++ show mat
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#show-aln-helpers>>[init]
-- | Pretty print an alignment.
-- This does not take terminal width into account, but simply wraps after 80 symbols.
showAln :: Aln -> String
showAln aln = (concat.breakAlnLines 80) [r1, syms, r2]
    where
      syms     = map tag aln
      (s1, s2) = unzip aln
      [r1, r2] = map (map toChar) [s1, s2]

-- | Pretty print a list of alignments. Wraps the same way as showAln does.
showAlns :: [Aln] -> String
showAlns = concat.(intersperse "\n\n\n").(map showAln)
-- ~/~ end
-- ~/~ begin <<doc/de/results/03_Implementation.md#show-aln>>[init]
-- | Produce the proper signifier for two aligned symbols.
-- I.e., '-' for gaps, '|' for matches, and '.' for missmatches.
tag :: (AlnChar, AlnChar) -> Char
tag (Gap, Gap) = '-'
tag (  _, Gap) = '-'
tag (Gap,   _) = '-'
tag (x, y)
  | x == y     = '|'
  | otherwise  = '.'

-- | Helper for calculating lines in the string representation of an Aln.
breakAlnLines :: Int -> [String] -> [String]
breakAlnLines _     []      = []
breakAlnLines width strings = partlines : breakAlnLines width rest
  where
    (parts, rests) = unzip $ map (splitAt width) strings
    rest           = filter (not.null) rests
    break          = concat.(intersperse "\n")
    end            = if null rest then "" else "\n\n"
    partlines      = break parts ++ end
-- ~/~ end
-- ~/~ end
-- ~/~ end