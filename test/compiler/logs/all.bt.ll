; ModuleID = 'Beathoven Codegen'

@tmp = private unnamed_addr constant [6 x i8] c"hello\00"
@fmt = private unnamed_addr constant [3 x i8] c"%s\00"
@fmt.1 = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.2 = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp.3 = private unnamed_addr constant [10 x i8] c"foo works\00"
@fmt.4 = private unnamed_addr constant [3 x i8] c"%s\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @tmp, i32 0, i32 0))
  %a = alloca i32
  store i32 0, i32* %a
  %a1 = load i32, i32* %a
  %tmp2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.1, i32 0, i32 0), i32 %a1)
  %b = alloca i32
  %a3 = load i32, i32* %a
  store i32 %a3, i32* %b
  %a4 = load i32, i32* %a
  %tmp5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.2, i32 0, i32 0), i32 %a4)
  call void @"~beathoven.foo"()
  ret void
}

define void @"~beathoven.bar"() {
entry:
  %a = alloca i32
  store i32 1, i32* %a
  ret void
}

define void @"~beathoven.foo"() {
entry:
  %a = alloca i32
  store i32 1, i32* %a
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.4, i32 0, i32 0), i8* getelementptr inbounds ([10 x i8], [10 x i8]* @tmp.3, i32 0, i32 0))
  ret void
}
