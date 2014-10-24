type Student = (Int, String)
type Teacher = (Int, String)

data GroupState = Active | Inactive deriving (Show)
data Group = SimpleGroup [Student] (Maybe Teacher) | EmptyGroup deriving (Eq, Show)

mkGroup :: Group
mkGroup = SimpleGroup (mkStudents ["Hans", "Henning", "Flemming"]) (Just (1, "Tina"))

mkStudents :: [String] -> [Student]
mkStudents names = zip [1..] names

getStudents :: Group -> [Student]
getStudents (SimpleGroup students _) = students
getStudents EmptyGroup = []

getTeacher :: Group -> Maybe Teacher
getTeacher (SimpleGroup _ teacher) = teacher
getTeacher EmptyGroup = Nothing

hasTeacher :: Group -> Bool
hasTeacher (SimpleGroup _ Nothing) = False
hasTeacher EmptyGroup = False
hasTeacher _ = True

getState :: Group -> GroupState
getState (SimpleGroup students _) = if length students < 3 then Inactive else Active

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
addStudent newStudent group@(SimpleGroup students teacher) =
  let newStudents = if length students < 6
                      then newStudent:students
                      else students
  in SimpleGroup newStudents teacher
addStudent student EmptyGroup = SimpleGroup [student] Nothing

removeStudent :: Student -> Group -> Group
removeStudent student (SimpleGroup students teacher) =
  let newStudents = filter (\st -> st /= student) students
  in (SimpleGroup newStudents teacher)
removeStudent student EmptyGroup = EmptyGroup

addTeacher :: Teacher -> Group -> Either String Group
addTeacher teacher group =
  if getTeacher group == Nothing
    then Right $ SimpleGroup (getStudents group) (Just teacher)
    else Left "The group is already assigned a teacher!"

removeTeacher :: Group -> Group
removeTeacher (SimpleGroup students _) = SimpleGroup students Nothing
removeTeacher EmptyGroup = EmptyGroup