package net.chibidevteam.semver.constraint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import net.chibidevteam.semver.constraint.ConstraintSign;

public class ConstraintSignTest {

    @Test
    public void getConstraint() {
        assertEquals(ConstraintSign.EQ, ConstraintSign.getConstaint(""));
        assertEquals(ConstraintSign.EQ, ConstraintSign.getConstaint("="));

        assertEquals(ConstraintSign.NOT, ConstraintSign.getConstaint("!"));

        assertEquals(ConstraintSign.LT, ConstraintSign.getConstaint("<"));

        assertEquals(ConstraintSign.LTE, ConstraintSign.getConstaint("<="));

        assertEquals(ConstraintSign.GT, ConstraintSign.getConstaint(">"));

        assertEquals(ConstraintSign.GTE, ConstraintSign.getConstaint(">="));

        assertEquals(ConstraintSign.COMPATIBLE, ConstraintSign.getConstaint("^"));

        assertEquals(ConstraintSign.NEXT_SIGNIFICANT, ConstraintSign.getConstaint("~"));
    }

    @Test
    public void getSigns() {
        assertTrue("EQ should have sign ''", ConstraintSign.EQ.getSigns().contains(""));
        assertTrue("EQ should have sign '='", ConstraintSign.EQ.getSigns().contains("="));

        assertTrue("NOT should have sign '!'", ConstraintSign.NOT.getSigns().contains("!"));

        assertTrue("LT should have sign '<'", ConstraintSign.LT.getSigns().contains("<"));

        assertTrue("LTE should have sign '<='", ConstraintSign.LTE.getSigns().contains("<="));

        assertTrue("GT should have sign '>'", ConstraintSign.GT.getSigns().contains(">"));

        assertTrue("GTE should have sign '>='", ConstraintSign.GTE.getSigns().contains(">="));

        assertTrue("COMPATIBLE should have sign '^'", ConstraintSign.COMPATIBLE.getSigns().contains("^"));

        assertTrue("NEXT_SIGNIFICANT should have sign '~'", ConstraintSign.NEXT_SIGNIFICANT.getSigns().contains("~"));
    }

}
