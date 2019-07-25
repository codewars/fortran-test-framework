module CW2
  implicit none
  private :: &
    assertInt32Eq, &
    assertInt32EqWithMsg, &
    assertInt64Eq, &
    assertInt64EqWithMsg, &
    assertInt128Eq, &
    assertInt128EqWithMsg, &
    assertBoolEq, &
    assertBoolEqWithMsg, &
    assertStrEq, &
    assertStrEqWithMsg, &
    assertInt32NEq, &
    assertInt32NEqWithMsg, &
    assertInt64NEq, &
    assertInt64NEqWithMsg, &
    assertInt128NEq, &
    assertInt128NEqWithMsg, &
    assertBoolNEq, &
    assertBoolNEqWithMsg, &
    assertStrNEq, &
    assertStrNEqWithMsg, &
    floatAssert, &
    floatAssertWithMsg, &
    doubleAssert, &
    doubleAssertWithMsg, &
    complexAssert, &
    complexAssertWithMsg, &
    floatInvAssert, &
    floatInvAssertWithMsg, &
    doubleInvAssert, &
    doubleInvAssertWithMsg, &
    complexInvAssert, &
    complexInvAssertWithMsg
  interface assertEquals
    module procedure &
      assertInt32Eq, &
      assertInt32EqWithMsg, &
      assertInt64Eq, &
      assertInt64EqWithMsg, &
      assertInt128Eq, &
      assertInt128EqWithMsg, &
      assertBoolEq, &
      assertBoolEqWithMsg, &
      assertStrEq, &
      assertStrEqWithMsg
  end interface assertEquals
  interface assertNotEquals
    module procedure &
      assertInt32NEq, &
      assertInt32NEqWithMsg, &
      assertInt64NEq, &
      assertInt64NEqWithMsg, &
      assertInt128NEq, &
      assertInt128NEqWithMsg, &
      assertBoolNEq, &
      assertBoolNEqWithMsg, &
      assertStrNEq, &
      assertStrNEqWithMsg
  end interface assertNotEquals
  interface assertWithinTolerance
    module procedure &
      floatAssert, &
      floatAssertWithMsg, &
      doubleAssert, &
      doubleAssertWithMsg, &
      complexAssert, &
      complexAssertWithMsg
  end interface assertWithinTolerance
  interface assertNotWithinTolerance
    module procedure &
      floatInvAssert, &
      floatInvAssertWithMsg, &
      doubleInvAssert, &
      doubleInvAssertWithMsg, &
      complexInvAssert, &
      complexInvAssertWithMsg
  end interface assertNotWithinTolerance
  contains
    subroutine describe(msg)
      implicit none
      character(len=*) :: msg
      character(len=100) :: formatSpecifier
      write(formatSpecifier, "(A2, I0, A1)") "(A", 13 + len(msg), ")"
      print formatSpecifier, char(10) // "<DESCRIBE::>" // msg
    end subroutine describe
    subroutine it(msg)
      implicit none
      character(len=*) :: msg
      character(len=100) :: formatSpecifier
      write(formatSpecifier, "(A2, I0, A1)") "(A", 7 + len(msg), ")"
      print formatSpecifier, char(10) // "<IT::>" // msg
    end subroutine it
    subroutine endContext()
      implicit none
      print "(A16)", char(10) // "<COMPLETEDIN::>"
    end subroutine endContext
    subroutine assertInt32Eq(expected, actual)
      implicit none
      integer :: expected, actual
      if (actual == expected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        print "(A21, I0, A15, I0)", char(10) // &
        "<FAILED::>Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertInt32Eq
    subroutine assertInt32EqWithMsg(expected, actual, msg)
      implicit none
      integer :: expected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual == expected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A13, I0, A15, I0)", char(10) // &
        "<FAILED::>", msg, " - Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertInt32EqWithMsg
    subroutine assertInt64Eq(expected, actual)
      implicit none
      integer(kind=8) :: expected, actual
      if (actual == expected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        print "(A21, I0, A15, I0)", char(10) // &
        "<FAILED::>Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertInt64Eq
    subroutine assertInt64EqWithMsg(expected, actual, msg)
      implicit none
      integer(kind=8) :: expected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual == expected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A13, I0, A15, I0)", char(10) // &
        "<FAILED::>", msg, " - Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertInt64EqWithMsg
    subroutine assertInt128Eq(expected, actual)
      implicit none
      integer(kind=16) :: expected, actual
      if (actual == expected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        print "(A21, I0, A15, I0)", char(10) // &
        "<FAILED::>Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertInt128Eq
    subroutine assertInt128EqWithMsg(expected, actual, msg)
      implicit none
      integer(kind=16) :: expected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual == expected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A13, I0, A15, I0)", char(10) // &
        "<FAILED::>", msg, " - Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertInt128EqWithMsg
    subroutine assertBoolEq(expected, actual)
      implicit none
      logical :: expected, actual
      if (actual .eqv. expected) then
        print "(A34, L1)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        print "(A21, L1, A15, L1)", char(10) // &
        "<FAILED::>Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertBoolEq
    subroutine assertBoolEqWithMsg(expected, actual, msg)
      implicit none
      logical :: expected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual .eqv. expected) then
        print "(A34, L1)", char(10) // "<PASSED::>Test Passed - Value == ", expected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A13, L1, A15, L1)", char(10) // &
        "<FAILED::>", msg, " - Expected: ", expected, ", instead got: ", actual
      end if
    end subroutine assertBoolEqWithMsg
    subroutine assertStrEq(expected, actual)
      implicit none
      character(len=*) :: expected, actual
      character(len=100) :: n, m
      write(n, "(I0)") len(expected)
      if (actual == expected .and. len(actual) == len(expected)) then
        if (len(expected) == 0) then
          print "(A50)", char(10) // "<PASSED::>Test Passed - Value was an empty string"
        else
          print "(A34, A" // n // ")", char(10) // "<PASSED::>Test Passed - Value == ", expected
        end if
      else
        write(m, "(I0)") len(actual)
        if (len(actual) == 0) then
          print "(A21, A" // n // ", A29)", char(10) // "<FAILED::>Expected: ", expected, ", instead got an empty string"
        else if (len(expected) == 0) then
          print "(A50, A" // m // ")", char(10) // "<FAILED::>Expected an empty string, instead got: ", actual
        else
          print "(A21, A" // n // ", A15, A" // m // ")", char(10) // &
          "<FAILED::>Expected: ", expected, ", instead got: ", actual
        end if
      end if
    end subroutine assertStrEq
    subroutine assertStrEqWithMsg(expected, actual, msg)
      implicit none
      character(len=*) :: expected, actual, msg
      character(len=100) :: n, m, o
      write(n, "(I0)") len(expected)
      if (actual == expected .and. len(actual) == len(expected)) then
        if (len(expected) == 0) then
          print "(A50)", char(10) // "<PASSED::>Test Passed - Value was an empty string"
        else
          print "(A34, A" // n // ")", char(10) // "<PASSED::>Test Passed - Value == ", expected
        end if
      else
        write(m, "(I0)") len(actual)
        write(o, "(I0)") len(msg)
        if (len(actual) == 0) then
          print "(A11, A" // o // ", A13, A" // n // ", A29)", char(10) // "<FAILED::>", msg, &
          " - Expected: ", expected, ", instead got an empty string"
        else if (len(expected) == 0) then
          print "(A11, A" // o // ", A42, A" // m // ")", char(10) // "<FAILED::>", msg, &
          " - Expected an empty string, instead got: ", actual
        else
          print "(A11, A" // o // ", A13, A" // n // ", A15, A" // m // ")", &
          char(10) // "<FAILED::>", msg, " - Expected: ", expected, ", instead got: ", actual
        end if
      end if
    end subroutine assertStrEqWithMsg
    subroutine assertInt32NEq(unexpected, actual)
      implicit none
      integer :: unexpected, actual
      if (actual /= unexpected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        print "(A41, I0)", char(10) // &
        "<FAILED::>Expected result to not equal: ", unexpected
      end if
    end subroutine assertInt32NEq
    subroutine assertInt32NEqWithMsg(unexpected, actual, msg)
      implicit none
      integer :: unexpected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual /= unexpected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A33, I0)", char(10) // &
        "<FAILED::>", msg, " - Expected result to not equal: ", unexpected
      end if
    end subroutine assertInt32NEqWithMsg
    subroutine assertInt64NEq(unexpected, actual)
      implicit none
      integer(kind=8) :: unexpected, actual
      if (actual /= unexpected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        print "(A41, I0)", char(10) // &
        "<FAILED::>Expected result to not equal: ", unexpected
      end if
    end subroutine assertInt64NEq
    subroutine assertInt64NEqWithMsg(unexpected, actual, msg)
      implicit none
      integer(kind=8) :: unexpected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual /= unexpected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A33, I0)", char(10) // &
        "<FAILED::>", msg, " - Expected result to not equal: ", unexpected
      end if
    end subroutine assertInt64NEqWithMsg
    subroutine assertInt128NEq(unexpected, actual)
      implicit none
      integer(kind=16) :: unexpected, actual
      if (actual /= unexpected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        print "(A41, I0)", char(10) // &
        "<FAILED::>Expected result to not equal: ", unexpected
      end if
    end subroutine assertInt128NEq
    subroutine assertInt128NEqWithMsg(unexpected, actual, msg)
      implicit none
      integer(kind=16) :: unexpected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual /= unexpected) then
        print "(A34, I0)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A33, I0)", char(10) // &
        "<FAILED::>", msg, " - Expected result to not equal: ", unexpected
      end if
    end subroutine assertInt128NEqWithMsg
    subroutine assertBoolNEq(unexpected, actual)
      implicit none
      logical :: unexpected, actual
      if (actual .neqv. unexpected) then
        print "(A34, L1)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        print "(A41, L1)", char(10) // &
        "<FAILED::>Expected result to not equal: ", unexpected
      end if
    end subroutine assertBoolNEq
    subroutine assertBoolNEqWithMsg(unexpected, actual, msg)
      implicit none
      logical :: unexpected, actual
      character(len=*) :: msg
      character(len=100) :: n
      if (actual .neqv. unexpected) then
        print "(A34, L1)", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A33, L1)", char(10) // &
        "<FAILED::>", msg, " - Expected result to not equal: ", unexpected
      end if
    end subroutine assertBoolNEqWithMsg
    subroutine assertStrNEq(unexpected, actual)
      implicit none
      character(len=*) :: unexpected, actual
      character(len=100) :: n
      write(n, "(I0)") len(unexpected)
      if (actual /= unexpected .or. len(actual) /= len(unexpected)) then
        if (len(unexpected) == 0) then
          print "(A53)", char(10) // "<PASSED::>Test Passed - Value is not an empty string"
        else
          print "(A34, A" // n // ")", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
        end if
      else
        if (len(unexpected) == 0) then
          print "(A55)", char(10) // "<FAILED::>Expected result to not equal an empty string"
        else
          print "(A41, A" // n // ")", char(10) // &
          "<FAILED::>Expected result to not equal: ", unexpected
        end if
      end if
    end subroutine assertStrNEq
    subroutine assertStrNEqWithMsg(unexpected, actual, msg)
      implicit none
      character(len=*) :: unexpected, actual, msg
      character(len=100) :: n, o
      write(n, "(I0)") len(unexpected)
      if (actual /= unexpected .or. len(actual) /= len(unexpected)) then
        if (len(unexpected) == 0) then
          print "(A53)", char(10) // "<PASSED::>Test Passed - Value is not an empty string"
        else
          print "(A34, A" // n // ")", char(10) // "<PASSED::>Test Passed - Value /= ", unexpected
        end if
      else
        write(o, "(I0)") len(msg)
        if (len(unexpected) == 0) then
          print "(A11, A" // o // ", A47)", char(10) // "<FAILED::>", msg, &
          " - Expected result to not equal an empty string"
        else
          print "(A11, A" // o // ", A33, A" // n // ")", char(10) // &
          "<FAILED::>", msg, " - Expected result to not equal: ", unexpected
        end if
      end if
    end subroutine assertStrNEqWithMsg
    subroutine floatAssert(expected, actual, epsilon)
      implicit none
      real :: expected, actual, epsilon
      character(len=100) :: expW, epsW, actW
      if (expected == 0.0) then
        write(expW, "(I0)") 8
      else
        write(expW, "(I0)") 8 + merge(0, 1, expected >= 0) + merge(floor(log10(abs(expected))), 0, abs(expected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - expected) <= epsilon) then
        print "(A34, F" // expW // ".6, A14, F" // epsW // ".6)", &
        char(10) // "<PASSED::>Test Passed - Value == ", expected, " within range ", epsilon
      else
        if (actual == 0.0) then
          write(actW, "(I0)") 8
        else
          write(actW, "(I0)") 8 + merge(0, 1, actual >= 0) + merge(floor(log10(abs(actual))), 0, abs(actual) >= 1)
        end if
        print "(A33, F" // actW // ".6, A24, F" // expW // ".6, A14, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>Failed asserting that ", actual, " matches expected value ", expected, " within range ", epsilon
      end if
    end subroutine floatAssert
    subroutine floatAssertWithMsg(expected, actual, epsilon, msg)
      implicit none
      real :: expected, actual, epsilon
      character(len=*) :: msg
      character(len=100) :: expW, epsW, actW, n
      if (expected == 0.0) then
        write(expW, "(I0)") 8
      else
        write(expW, "(I0)") 8 + merge(0, 1, expected >= 0) + merge(floor(log10(abs(expected))), 0, abs(expected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - expected) <= epsilon) then
        print "(A34, F" // expW // ".6, A14, F" // epsW // ".6)", &
        char(10) // "<PASSED::>Test Passed - Value == ", expected, " within range ", epsilon
      else
        if (actual == 0.0) then
          write(actW, "(I0)") 8
        else
          write(actW, "(I0)") 8 + merge(0, 1, actual >= 0) + merge(floor(log10(abs(actual))), 0, abs(actual) >= 1)
        end if
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A25, F" // actW // ".6, A24, F" // expW // ".6, A14, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>", msg, " - Failed asserting that ", actual, " matches expected value ", expected, " within range ", epsilon
      end if
    end subroutine floatAssertWithMsg
    subroutine doubleAssert(expected, actual, epsilon)
      implicit none
      real(kind=8) :: expected, actual, epsilon
      character(len=100) :: expW, epsW, actW
      if (expected == 0.0) then
        write(expW, "(I0)") 17
      else
        write(expW, "(I0)") 17 + merge(0, 1, expected >= 0) + merge(floor(log10(abs(expected))), 0, abs(expected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 17
      else
        write(epsW, "(I0)") 17 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - expected) <= epsilon) then
        print "(A34, F" // expW // ".15, A14, F" // epsW // ".15)", &
        char(10) // "<PASSED::>Test Passed - Value == ", expected, " within range ", epsilon
      else
        if (actual == 0.0) then
          write(actW, "(I0)") 17
        else
          write(actW, "(I0)") 17 + merge(0, 1, actual >= 0) + merge(floor(log10(abs(actual))), 0, abs(actual) >= 1)
        end if
        print "(A33, F" // actW // ".15, A24, F" // expW // ".15, A14, F" // epsW // ".15)", &
        char(10) // &
        "<FAILED::>Failed asserting that ", actual, " matches expected value ", expected, " within range ", epsilon
      end if
    end subroutine doubleAssert
    subroutine doubleAssertWithMsg(expected, actual, epsilon, msg)
      implicit none
      real(kind=8) :: expected, actual, epsilon
      character(len=*) :: msg
      character(len=100) :: expW, epsW, actW, n
      if (expected == 0.0) then
        write(expW, "(I0)") 17
      else
        write(expW, "(I0)") 17 + merge(0, 1, expected >= 0) + merge(floor(log10(abs(expected))), 0, abs(expected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 17
      else
        write(epsW, "(I0)") 17 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - expected) <= epsilon) then
        print "(A34, F" // expW // ".15, A14, F" // epsW // ".15)", &
        char(10) // "<PASSED::>Test Passed - Value == ", expected, " within range ", epsilon
      else
        if (actual == 0.0) then
          write(actW, "(I0)") 17
        else
          write(actW, "(I0)") 17 + merge(0, 1, actual >= 0) + merge(floor(log10(abs(actual))), 0, abs(actual) >= 1)
        end if
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A25, F" // actW // ".15, A24, F" // expW // ".15, A14, F" // epsW // ".15)", &
        char(10) // &
        "<FAILED::>", msg, " - Failed asserting that ", actual, " matches expected value ", expected, " within range ", epsilon
      end if
    end subroutine doubleAssertWithMsg
    subroutine complexAssert(expected, actual, epsilon)
      implicit none
      complex :: expected, actual
      real :: epsilon
      character(len=100) :: expRealPartW, expImagPartW, epsW, actRealPartW, actImagPartW
      if (realpart(expected) == 0.0) then
        write(expRealPartW, "(I0)") 8
      else
        write(expRealPartW, "(I0)") 8 + merge(0, 1, realpart(expected) >= 0) + &
        merge(floor(log10(abs(realpart(expected)))), 0, abs(realpart(expected)) >= 1)
      end if
      if (imagpart(expected) == 0.0) then
        write(expImagPartW, "(I0)") 8
      else
        write(expImagPartW, "(I0)") 8 + merge(0, 1, imagpart(expected) >= 0) + &
        merge(floor(log10(abs(imagpart(expected)))), 0, abs(imagpart(expected)) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - expected) <= epsilon) then
        print "(A35, F" // expRealPartW // ".6, A2, F" // expImagPartW // ".6, A15, F" // epsW // ".6)", &
        char(10) // &
        "<PASSED::>Test Passed - Value == (", realpart(expected), ", ", imagpart(expected), ") within range ", epsilon
      else
        if (realpart(actual) == 0.0) then
          write(actRealPartW, "(I0)") 8
        else
          write(actRealPartW, "(I0)") 8 + merge(0, 1, realpart(actual) >= 0) + &
          merge(floor(log10(abs(realpart(actual)))), 0, abs(realpart(actual)) >= 1)
        end if
        if (imagpart(actual) == 0.0) then
          write(actImagPartW, "(I0)") 8
        else
          write(actImagPartW, "(I0)") 8 + merge(0, 1, imagpart(actual) >= 0) + &
          merge(floor(log10(abs(imagpart(actual)))), 0, abs(imagpart(actual)) >= 1)
        end if
        print "(A34, F" // actRealPartW // ".6, A2, F" // actImagPartW // &
        ".6, A26, F" // expRealPartW // ".6, A2, F" // expImagPartW // ".6, A15, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>Failed asserting that (", realpart(actual), ", ", imagpart(actual), ") matches expected value (", &
        realpart(expected), ", ", imagpart(expected), ") within range ", epsilon
      end if
    end subroutine complexAssert
    subroutine complexAssertWithMsg(expected, actual, epsilon, msg)
      implicit none
      complex :: expected, actual
      real :: epsilon
      character(len=*) :: msg
      character(len=100) :: expRealPartW, expImagPartW, epsW, actRealPartW, actImagPartW, n
      if (realpart(expected) == 0.0) then
        write(expRealPartW, "(I0)") 8
      else
        write(expRealPartW, "(I0)") 8 + merge(0, 1, realpart(expected) >= 0) + &
        merge(floor(log10(abs(realpart(expected)))), 0, abs(realpart(expected)) >= 1)
      end if
      if (imagpart(expected) == 0.0) then
        write(expImagPartW, "(I0)") 8
      else
        write(expImagPartW, "(I0)") 8 + merge(0, 1, imagpart(expected) >= 0) + &
        merge(floor(log10(abs(imagpart(expected)))), 0, abs(imagpart(expected)) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - expected) <= epsilon) then
        print "(A35, F" // expRealPartW // ".6, A2, F" // expImagPartW // ".6, A15, F" // epsW // ".6)", &
        char(10) // &
        "<PASSED::>Test Passed - Value == (", realpart(expected), ", ", imagpart(expected), ") within range ", epsilon
      else
        if (realpart(actual) == 0.0) then
          write(actRealPartW, "(I0)") 8
        else
          write(actRealPartW, "(I0)") 8 + merge(0, 1, realpart(actual) >= 0) + &
          merge(floor(log10(abs(realpart(actual)))), 0, abs(realpart(actual)) >= 1)
        end if
        if (imagpart(actual) == 0.0) then
          write(actImagPartW, "(I0)") 8
        else
          write(actImagPartW, "(I0)") 8 + merge(0, 1, imagpart(actual) >= 0) + &
          merge(floor(log10(abs(imagpart(actual)))), 0, abs(imagpart(actual)) >= 1)
        end if
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A26, F" // actRealPartW // ".6, A2, F" // actImagPartW // &
        ".6, A26, F" // expRealPartW // ".6, A2, F" // expImagPartW // ".6, A15, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>", msg, " - Failed asserting that (", realpart(actual), ", ", imagpart(actual), ") matches expected value (", &
        realpart(expected), ", ", imagpart(expected), ") within range ", epsilon
      end if
    end subroutine complexAssertWithMsg
    subroutine floatInvAssert(unexpected, actual, epsilon)
      implicit none
      real :: unexpected, actual, epsilon
      character(len=100) :: unexpW, epsW
      if (unexpected == 0.0) then
        write(unexpW, "(I0)") 8
      else
        write(unexpW, "(I0)") 8 + merge(0, 1, unexpected >= 0) + merge(floor(log10(abs(unexpected))), 0, abs(unexpected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - unexpected) > epsilon) then
        print "(A34, F" // unexpW // ".6, A18, F" // epsW // ".6, A1)", &
        char(10) // &
        "<PASSED::>Test Passed - Value /= ", unexpected, " (rejected range: ", epsilon, ")"
      else
        print "(A46, F" // unexpW // ".6, A14, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>Result should not be equivalent to ", unexpected, " within range ", epsilon
      end if
    end subroutine floatInvAssert
    subroutine floatInvAssertWithMsg(unexpected, actual, epsilon, msg)
      implicit none
      real :: unexpected, actual, epsilon
      character(len=*) :: msg
      character(len=100) :: unexpW, epsW, n
      if (unexpected == 0.0) then
        write(unexpW, "(I0)") 8
      else
        write(unexpW, "(I0)") 8 + merge(0, 1, unexpected >= 0) + merge(floor(log10(abs(unexpected))), 0, abs(unexpected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - unexpected) > epsilon) then
        print "(A34, F" // unexpW // ".6, A18, F" // epsW // ".6, A1)", &
        char(10) // &
        "<PASSED::>Test Passed - Value /= ", unexpected, " (rejected range: ", epsilon, ")"
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A38, F" // unexpW // ".6, A14, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>", msg, " - Result should not be equivalent to ", unexpected, " within range ", epsilon
      end if
    end subroutine floatInvAssertWithMsg
    subroutine doubleInvAssert(unexpected, actual, epsilon)
      implicit none
      real(kind=8) :: unexpected, actual, epsilon
      character(len=100) :: unexpW, epsW
      if (unexpected == 0.0) then
        write(unexpW, "(I0)") 17
      else
        write(unexpW, "(I0)") 17 + merge(0, 1, unexpected >= 0) + merge(floor(log10(abs(unexpected))), 0, abs(unexpected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 17
      else
        write(epsW, "(I0)") 17 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - unexpected) > epsilon) then
        print "(A34, F" // unexpW // ".15, A18, F" // epsW // ".15, A1)", &
        char(10) // &
        "<PASSED::>Test Passed - Value /= ", unexpected, " (rejected range: ", epsilon, ")"
      else
        print "(A46, F" // unexpW // ".15, A14, F" // epsW // ".15)", &
        char(10) // &
        "<FAILED::>Result should not be equivalent to ", unexpected, " within range ", epsilon
      end if
    end subroutine doubleInvAssert
    subroutine doubleInvAssertWithMsg(unexpected, actual, epsilon, msg)
      implicit none
      real(kind=8) :: unexpected, actual, epsilon
      character(len=*) :: msg
      character(len=100) :: unexpW, epsW, n
      if (unexpected == 0.0) then
        write(unexpW, "(I0)") 17
      else
        write(unexpW, "(I0)") 17 + merge(0, 1, unexpected >= 0) + merge(floor(log10(abs(unexpected))), 0, abs(unexpected) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 17
      else
        write(epsW, "(I0)") 17 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - unexpected) > epsilon) then
        print "(A34, F" // unexpW // ".15, A18, F" // epsW // ".15, A1)", &
        char(10) // &
        "<PASSED::>Test Passed - Value /= ", unexpected, " (rejected range: ", epsilon, ")"
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A38, F" // unexpW // ".15, A14, F" // epsW // ".15)", &
        char(10) // &
        "<FAILED::>", msg, " - Result should not be equivalent to ", unexpected, " within range ", epsilon
      end if
    end subroutine doubleInvAssertWithMsg
    subroutine complexInvAssert(unexpected, actual, epsilon)
      implicit none
      complex :: unexpected, actual
      real :: epsilon
      character(len=100) :: unexpRealPartW, unexpImagPartW, epsW
      if (realpart(unexpected) == 0.0) then
        write(unexpRealPartW, "(I0)") 8
      else
        write(unexpRealPartW, "(I0)") 8 + merge(0, 1, realpart(unexpected) >= 0) + &
        merge(floor(log10(abs(realpart(unexpected)))), 0, abs(realpart(unexpected)) >= 1)
      end if
      if (imagpart(unexpected) == 0.0) then
        write(unexpImagPartW, "(I0)") 8
      else
        write(unexpImagPartW, "(I0)") 8 + merge(0, 1, imagpart(unexpected) >= 0) + &
        merge(floor(log10(abs(imagpart(unexpected)))), 0, abs(imagpart(unexpected)) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - unexpected) > epsilon) then
        print "(A35, F" // unexpRealPartW // ".6, A2, F" // unexpImagPartW // ".6, A19, F" // epsW // ".6, A1)", &
        char(10) // &
        "<PASSED::>Test Passed - Value /= (", realpart(unexpected), ", ", imagpart(unexpected), ") (rejected range: ", epsilon, ")"
      else
        print "(A47, F" // unexpRealPartW // ".6, A2, F" // unexpImagPartW // ".6, A15, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>Result should not be equivalent to (", realpart(unexpected), &
        ", ", imagpart(unexpected), ") within range ", epsilon
      end if
    end subroutine complexInvAssert
    subroutine complexInvAssertWithMsg(unexpected, actual, epsilon, msg)
      implicit none
      complex :: unexpected, actual
      real :: epsilon
      character(len=*) :: msg
      character(len=100) :: unexpRealPartW, unexpImagPartW, epsW, n
      if (realpart(unexpected) == 0.0) then
        write(unexpRealPartW, "(I0)") 8
      else
        write(unexpRealPartW, "(I0)") 8 + merge(0, 1, realpart(unexpected) >= 0) + &
        merge(floor(log10(abs(realpart(unexpected)))), 0, abs(realpart(unexpected)) >= 1)
      end if
      if (imagpart(unexpected) == 0.0) then
        write(unexpImagPartW, "(I0)") 8
      else
        write(unexpImagPartW, "(I0)") 8 + merge(0, 1, imagpart(unexpected) >= 0) + &
        merge(floor(log10(abs(imagpart(unexpected)))), 0, abs(imagpart(unexpected)) >= 1)
      end if
      if (epsilon == 0.0) then
        write(epsW, "(I0)") 8
      else
        write(epsW, "(I0)") 8 + merge(0, 1, epsilon >= 0) + merge(floor(log10(abs(epsilon))), 0, abs(epsilon) >= 1)
      end if
      if (abs(actual - unexpected) > epsilon) then
        print "(A35, F" // unexpRealPartW // ".6, A2, F" // unexpImagPartW // ".6, A19, F" // epsW // ".6, A1)", &
        char(10) // &
        "<PASSED::>Test Passed - Value /= (", realpart(unexpected), ", ", imagpart(unexpected), ") (rejected range: ", epsilon, ")"
      else
        write(n, "(I0)") len(msg)
        print "(A11, A" // n // ", A39, F" // unexpRealPartW // ".6, A2, F" // unexpImagPartW // ".6, A15, F" // epsW // ".6)", &
        char(10) // &
        "<FAILED::>", msg, " - Result should not be equivalent to (", realpart(unexpected), &
        ", ", imagpart(unexpected), ") within range ", epsilon
      end if
    end subroutine complexInvAssertWithMsg
end module CW2
