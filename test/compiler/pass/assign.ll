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
  %a1 = load i32, i32* %a
  %tmp = add i32 %a1, 2
  store i32 %tmp, i32* %b
  %tmp2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt, i32 0, i32 0), i32 1)
  %tmp3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.1, i32 0, i32 0), i1 false)
  %b4 = load i32, i32* %b
  %tmp5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.2, i32 0, i32 0), i32 %b4)
  %a6 = load i32, i32* %a
  %b7 = load i32, i32* %b
  %tmp8 = add i32 %a6, %b7
  %tmp9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.3, i32 0, i32 0), i32 %tmp8)
  ret void
}