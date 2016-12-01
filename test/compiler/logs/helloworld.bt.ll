; ModuleID = 'Beathoven Codegen'

@tmp = private unnamed_addr constant [12 x i8] c"hello world\00"
@fmt = private unnamed_addr constant [3 x i8] c"%s\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([12 x i8], [12 x i8]* @tmp, i32 0, i32 0))
  ret void
}
