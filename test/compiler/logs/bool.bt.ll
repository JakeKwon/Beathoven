; ModuleID = 'Beathoven Codegen'

@fmt = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.1 = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.2 = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.3 = private unnamed_addr constant [3 x i8] c"%d\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  %x = alloca i1
  store i1 false, i1* %x
  %y = alloca i1
  store i1 true, i1* %y
  %x1 = load i1, i1* %x
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt, i32 0, i32 0), i1 %x1)
  %tmp2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.1, i32 0, i32 0), i32 1)
  %y3 = load i1, i1* %y
  %tmp4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.2, i32 0, i32 0), i1 %y3)
  %x5 = load i1, i1* %x
  store i1 %x5, i1* %y
  %y6 = load i1, i1* %y
  %tmp7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.3, i32 0, i32 0), i1 %y6)
  ret void
}
