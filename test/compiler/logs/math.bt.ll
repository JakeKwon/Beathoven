; ModuleID = 'Beathoven Codegen'

@fmt = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.1 = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.2 = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.3 = private unnamed_addr constant [3 x i8] c"%d\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  %a = alloca i32
  store i32 1, i32* %a
  %b = alloca i32
  store i32 4, i32* %b
  %c = alloca i32
  store i32 4, i32* %c
  %a1 = load i32, i32* %a
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt, i32 0, i32 0), i32 %a1)
  %tmp2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.1, i32 0, i32 0), i1 false)
  %b3 = load i32, i32* %b
  %tmp4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.2, i32 0, i32 0), i32 %b3)
  %c5 = load i32, i32* %c
  %tmp6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.3, i32 0, i32 0), i32 %c5)
  ret void
}
