package net.chibidevteam.semver.constraint;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import net.chibidevteam.semver.Version;
import net.chibidevteam.semver.constraint.Constraint;
import net.chibidevteam.semver.constraint.ConstraintHelper;
import net.chibidevteam.semver.constraint.ConstraintSign;
import net.chibidevteam.semver.exceptions.MalformedVersionException;

public class ConstraintHelperTest {

    @Test
    public void isEqualConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.EQ, "1.2"));
        assertTrue(ConstraintHelper.is(ConstraintSign.EQ, "=1.2"));

        assertFalse(ConstraintHelper.is(ConstraintSign.EQ, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.EQ, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.EQ, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.EQ, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.EQ, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.EQ, "^1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.EQ, "~1.2"));

        assertTrue(ConstraintHelper.isEqualConstraint("1.2"));
        assertTrue(ConstraintHelper.isEqualConstraint("=1.2"));
    }

    @Test
    public void isNegationConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.NOT, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, "1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, "=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, "^1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NOT, "~1.2"));

        assertTrue(ConstraintHelper.isNegationConstraint("!1.2"));
    }

    @Test
    public void isLowerConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.LT, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, "1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, "=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, "^1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LT, "~1.2"));

        assertTrue(ConstraintHelper.isLowerConstraint("<1.2"));
    }

    @Test
    public void isLowerOrEqualConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.LTE, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, "1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, "=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, "^1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.LTE, "~1.2"));

        assertTrue(ConstraintHelper.isLowerOrEqualConstraint("<=1.2"));
    }

    @Test
    public void isGreaterConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.GT, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, "1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, "=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, "^1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GT, "~1.2"));

        assertTrue(ConstraintHelper.isGreaterConstraint(">1.2"));
    }

    @Test
    public void isGreaterOrEqualConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.GTE, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, "1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, "=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, "^1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.GTE, "~1.2"));

        assertTrue(ConstraintHelper.isGreaterOrEqualConstraint(">=1.2"));
    }

    @Test
    public void isCompatibilityConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.COMPATIBLE, "^1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, "1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, "=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.COMPATIBLE, "~1.2"));

        assertTrue(ConstraintHelper.isCompatibilityConstraint("^1.2"));
    }

    @Test
    public void isNextSignificantConstraint() {
        assertTrue(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, "~1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, "1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, "=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, "!1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, ">1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, ">=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, "<1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, "<=1.2"));
        assertFalse(ConstraintHelper.is(ConstraintSign.NEXT_SIGNIFICANT, "^1.2"));

        assertTrue(ConstraintHelper.isNextSignificantConstraint("~1.2"));
    }

    @Test
    public void matches() {
        try {
            assertTrue(ConstraintHelper.matches("0.2", "0.2"));
            assertTrue(ConstraintHelper.matches("=0.2", "0.2"));
            assertFalse(ConstraintHelper.matches("=0.2", "1.2"));

            assertTrue(ConstraintHelper.matches("!1.2-RC1", "1.2-RC2"));
            assertFalse(ConstraintHelper.matches("!1.2-RC1", "1.2-RC1"));

            assertTrue(ConstraintHelper.matches(">1.2-RC1", "1.2"));
            assertFalse(ConstraintHelper.matches(">1.2-RC1", "1.2-alpha"));

            assertTrue(ConstraintHelper.matches(">=1.2-RC1", "1.2"));
            assertTrue(ConstraintHelper.matches(">=1.2-RC1", "1.2-RC1"));
            assertFalse(ConstraintHelper.matches(">=1.2-RC1", "1.2-alpha"));

            assertTrue(ConstraintHelper.matches("<1.2-RC1", "1.2-alpha"));
            assertFalse(ConstraintHelper.matches("<1.2-RC1", "1.2"));

            assertTrue(ConstraintHelper.matches("<=1.2-RC1", "1.2-alpha"));
            assertTrue(ConstraintHelper.matches("<=1.2-RC1", "1.2-RC1"));
            assertFalse(ConstraintHelper.matches("<=1.2-RC1", "1.2"));

            assertTrue(ConstraintHelper.matches("^0.2", "0.2.5"));
            assertFalse(ConstraintHelper.matches("^0.2", "0.3"));
            assertTrue(ConstraintHelper.matches("^1.2", "1.9.3"));

            assertTrue(ConstraintHelper.matches("~0.2", "0.2.5"));
            assertTrue(ConstraintHelper.matches("~0.2", "0.3"));
            assertTrue(ConstraintHelper.matches("~1.2", "1.9.3"));
            assertFalse(ConstraintHelper.matches("~1.9.3", "1.10"));

            assertTrue(ConstraintHelper.matches("^1.2 !1.5", "1.4"));
            assertFalse(ConstraintHelper.matches("^1.2 !1.5", "1.5"));
            assertTrue(ConstraintHelper.matches("^1.2 !1.5 || ~3.2", "1.6"));
            assertTrue(ConstraintHelper.matches("^1.2 !1.5 || ~3.2", "3.5.2"));

            assertTrue(ConstraintHelper.matches(new Constraint(ConstraintSign.COMPATIBLE, new Version(1, 5, 2, null)), "1.8"));
            assertTrue(ConstraintHelper.matches("^1.5.8", new Version(1, 8, null, null)));
        } catch (MalformedVersionException e) {
            fail("Check VersionHelper: " + e.getMessage());
        }
    }

}
