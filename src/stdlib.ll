; ModuleID = 'stdlib.c'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.10.0"

%struct.pitch = type { i8, i32, i32 }

@p = common global %struct.pitch zeroinitializer, align 4

; Function Attrs: nounwind ssp uwtable
define void @g() #0 {
  %1 = alloca { i64, i32 }
  %2 = bitcast { i64, i32 }* %1 to i8*
  %3 = bitcast %struct.pitch* @p to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %2, i8* %3, i64 12, i32 0, i1 false)
  %4 = getelementptr { i64, i32 }* %1, i32 0, i32 0
  %5 = load i64* %4, align 1
  %6 = getelementptr { i64, i32 }* %1, i32 0, i32 1
  %7 = load i32* %6, align 1
  call void @f(i64 %5, i32 %7)
  ret void
}

declare void @f(i64, i32) #1

; Function Attrs: nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1) #2

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+ssse3,+cx16,+sse,+sse2,+sse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+ssse3,+cx16,+sse,+sse2,+sse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"PIC Level", i32 2}
!1 = !{!"Apple LLVM version 7.0.0 (clang-700.0.72)"}
