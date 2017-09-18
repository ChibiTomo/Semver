package net.chibidevteam.semver.comparision;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ConstraintTest {

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

}
