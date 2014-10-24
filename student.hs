import Data.List
import Data.Maybe

type Student = (Int, String)
type Teacher = (Int, String)

data GroupState = Active | Inactive deriving (Show)
data Group = SimpleGroup [Student] (Maybe Teacher) | EmptyGroup deriving (Eq, Show)

-- Getter for SimpleGroup students
getStudents :: Group -> [Student]
getStudents EmptyGroup = []
getStudents (SimpleGroup students _) = students

-- Getter for SimpleGroup teacher
getTeacher :: Group -> Maybe Teacher
getTeacher EmptyGroup = Nothing
getTeacher (SimpleGroup _ teacher) = teacher

-- Makers
mkGroup :: Group
mkGroup = SimpleGroup (mkStudents ["Hans", "Henning", "Flemming"]) (Just (1, "Tina"))

mkStudents :: [String] -> [Student]
mkStudents names = zip [1..] names

hasTeacher :: Group -> Bool
hasTeacher EmptyGroup = False
hasTeacher (SimpleGroup _ Nothing) = False
hasTeacher _ = True

-- |Returns whether the group is active or inactive.
getState :: Group -> GroupState
getState (SimpleGroup students _) = if length students < 3 then Inactive else Active

addStudents :: [Student] -> Group -> Group
addStudents studentsToAdd group =
  let studentsInGroup = getStudents group
      teacher = getTeacher group
      allStudents = foldl fn studentsInGroup studentsToAdd
      fn acc val = if length acc >= 6 -- If we have exactly 6 students in a group, don't add anymore
                    then acc
                    else val:acc
  in SimpleGroup allStudents teacher

addStudentsByName :: [String] -> Group -> Group
addStudentsByName studentsToAdd group = 
  let studentsInGroup = getStudents group
      teacher = getTeacher group
      allStudents = foldl fn studentsInGroup studentsToAdd
      fn acc val = if length acc >= 6 -- If we have exactly 6 students in a group, don't add anymore
                    then acc
                    else (1 + (length acc), val) : acc
  in SimpleGroup allStudents teacher

addStudent :: Student -> Group -> Group
addStudent student EmptyGroup = SimpleGroup [student] Nothing
addStudent newStudent (SimpleGroup students teacher) =
  let newStudents = if length students < 6
                      then newStudent:students
                      else students
  in SimpleGroup newStudents teacher

removeStudentById :: Int -> Group -> Group
removeStudentById _ EmptyGroup = EmptyGroup
removeStudentById studentId (SimpleGroup students teacher) =
  let updatedStudents = filter (\st -> fst st /= studentId) students
  in SimpleGroup updatedStudents teacher

removeStudent :: Student -> Group -> Group
removeStudent student = removeStudentById (fst student)

addTeacher :: Teacher -> Group -> Either String Group
addTeacher teacher group =
  if getTeacher group == Nothing
    then Right $ SimpleGroup (getStudents group) (Just teacher)
    else Left "The group is already assigned a teacher!"

removeTeacher :: Group -> Group
removeTeacher EmptyGroup = EmptyGroup
removeTeacher (SimpleGroup students _) = SimpleGroup students Nothing