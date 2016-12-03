; ModuleID = 'Beathoven Codegen'

@fmt = private unnamed_addr constant [3 x i8] c"%d\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  call void @"~beathoven.printHello"()
  call void @"~beathoven.printHello"()
  ret void
}

define void @"~beathoven.printHello"() {
entry:
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt, i32 0, i32 0), i32 5)
  ret void
}
