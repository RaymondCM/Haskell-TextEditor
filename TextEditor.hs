import Data.List
import System.IO

data Text = Text String String String String deriving (Show)

x = Text "The quick brown " "fox jumped over the lazy dog." "" ""

create :: Text
create = Text [] [] [] []

initilise :: Text -> Text
initilise (Text l r s b) = Text l r s b

insertChar :: Text -> Char -> Text
insertChar (Text l r s b) c = Text (l ++ [c]) r s b

del :: Text -> Text
del (Text l (r:rr) s b) = Text l rr [] b

backspace :: Text -> Text
backspace (Text l r s b) = Text (init l) r [] b

left :: Text -> Text
left (Text l r s b) = Text (init l) (((last l):s) ++ r) [] b

right :: Text -> Text
right (Text l (r:rr) s b) = Text (l ++ s ++ [r]) rr [] b

start :: Text -> Text
start (Text l r s b) = Text [] (l ++ s ++ r) [] b

end :: Text -> Text
end (Text l r s b) = Text (l ++ s ++ r) [] [] b

nextWord :: Text -> Text
nextWord (Text l (r:rr) s b) = nextWordIndex (Text (l ++ s ++ [r]) rr [] b)
  where
    nextWordIndex (Text l [] s b) = Text l [] s b
    nextWordIndex (Text l (r:rr) s b) = if (last l == ' ' && r /= ' ')
      then Text l (r:rr) s b else nextWordIndex (Text (l ++ [r]) rr s b)

lastWord :: Text -> Text
lastWord (Text l r s b) = lastWordIndex(Text (init l) ((last l):s ++ r) [] b)
  where
    lastWordIndex (Text [] r s b) = Text [] r s b
    lastWordIndex (Text l (r:rr) s b) = if (last l == ' ' && r /= ' ')
      then Text l (r:rr) s b else lastWordIndex(Text (init l) ((last l):(r:rr)) s b)

selectLeft :: Text -> Text
selectLeft (Text l r s b) = Text (init l) r ((last l):s) b

selectRight :: Text -> Text
selectRight (Text l (r:rr) s b) = Text l rr (s ++ [r]) b

selectNextWord :: Text -> Text
selectNextWord (Text l (r:rr) s b) = indexNextWord(Text l rr (s ++ [r]) b)
  where
    indexNextWord (Text l [] s b) = Text l [] s b
    indexNextWord (Text l (r:rr) s b) = if (last s == ' ' && r /= ' ')
      then Text l (r:rr) s b else indexNextWord(Text l rr (s ++ [r]) b)

selectLastWord :: Text -> Text
selectLastWord (Text l r s b) = indexLastWord(Text (init l) r ((last l):s) b)
  where
    indexLastWord (Text l [] s b) = Text l [] s b
    indexLastWord (Text l (r:rr) s b) = if (last l == ' ' && r /= ' ')
      then Text l (r:rr) s b else indexLastWord(Text (init l) (r:rr) ((last l):s) b)

selectToStart :: Text -> Text
selectToStart (Text l r s b) = Text [] r (l ++ s) b

selectToEnd :: Text -> Text
selectToEnd (Text l r s b) = Text l [] (s ++ r) b

selectAll :: Text -> Text
selectAll (Text l r s b) = Text [] [] (l ++ s ++ r) b

copy :: Text -> Text
copy (Text l r s b) = Text l r s s

paste :: Text -> Text
paste (Text l r _ b) = Text (l ++ b) r [] b

cut :: Text -> Text
cut (Text l r s _) = Text l r [] s

outputText :: Text -> String
outputText (Text l r s _) = l ++ s ++ r
