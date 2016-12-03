; ModuleID = 'Beathoven Codegen'

@tmp = private unnamed_addr constant [6 x i8] c"hello\00"
@fmt = private unnamed_addr constant [3 x i8] c"%s\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  call void @"~beathoven.printHello"()
  call void @"~beathoven.printHello"()
  ret void
}

define void @"~beathoven.printHello"() {
entry:
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @tmp, i32 0, i32 0))
  ret void
}
