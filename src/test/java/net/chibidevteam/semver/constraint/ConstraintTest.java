package net.chibidevteam.semver.constraint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import net.chibidevteam.semver.Version;

public class ConstraintTest {

    @Test
    public void ctor() {
        Constraint c = new Constraint();
        assertNull("ConstraintSign should be null", c.getConstraintSign());
        assertNull("Version should be null", c.getVersion());
        assertTrue("Ands should be empty", c.getAnds().isEmpty());
        assertTrue("Ors should be empty", c.getAnds().isEmpty());

        c = new Constraint(ConstraintSign.EQ, new Version(1, 2, 3, null));
        assertEquals(ConstraintSign.EQ, c.getConstraintSign());
        assertEquals(new Version(1, 2, 3, null), c.getVersion());
        assertTrue("Ands should be empty", c.getAnds().isEmpty());
        assertTrue("Ors should be empty", c.getAnds().isEmpty());
    }

    @Test
    public void adds() {
        Constraint c1 = new Constraint(ConstraintSign.EQ, new Version(1, 2, 3, null));
        Constraint c2 = new Constraint(ConstraintSign.EQ, new Version(5, 0, 8, null));
        Constraint c3 = new Constraint(ConstraintSign.NOT, new Version(1, 2, 3, null));

        c1.addAnd(c2);
        assertEquals(1, c1.getAnds().size());

        c1.addOr(c3);
        assertEquals(1, c1.getOrs().size());
    }

    @Test
    public void matches() {
        Version v1_5 = new Version(1, 5, null, null);
        Version v1_12 = new Version(1, 12, null, null);
        Version v1_8 = new Version(1, 8, null, null);
        Version v1_30 = new Version(1, 30, null, null);
        Version v1_2_9 = new Version(1, 2, 9, null);
        Version v2_5 = new Version(2, 5, null, null);
        Version v0_2_3 = new Version(0, 2, 3, null);
        Version v0_3 = new Version(0, 3, null, null);
        Version v1 = new Version(1, null, null, null);

        Constraint c = new Constraint(null, new Version(1, 2, null, null));
        assertTrue("null ConstraintSign should force matches", c.matches(v1_5));
        c = new Constraint(ConstraintSign.EQ, null);
        assertTrue("null Version should force matches", c.matches(v1_5));
        c = new Constraint(null, null);
        assertTrue("Both null ConstraintSign and Version should force matches", c.matches(v1_5));

        Constraint c1 = new Constraint(ConstraintSign.EQ, new Version(1, 5, null, null));
        Constraint c2 = new Constraint(ConstraintSign.NOT, new Version(1, 5, null, null));
        Constraint c3 = new Constraint(ConstraintSign.GT, new Version(1, 12, null, null));
        Constraint c4 = new Constraint(ConstraintSign.GTE, new Version(1, 12, null, null));
        Constraint c5 = new Constraint(ConstraintSign.LT, new Version(1, 12, null, null));
        Constraint c6 = new Constraint(ConstraintSign.LTE, new Version(1, 12, null, null));
        Constraint c7 = new Constraint(ConstraintSign.COMPATIBLE, new Version(1, 2, null, null));
        Constraint c7_1 = new Constraint(ConstraintSign.COMPATIBLE, new Version(0, 2, null, null));
        Constraint c7_2 = new Constraint(ConstraintSign.COMPATIBLE, new Version(0, null, null, null));
        Constraint c8 = new Constraint(ConstraintSign.NEXT_SIGNIFICANT, new Version(1, 2, null, null));
        Constraint c8_1 = new Constraint(ConstraintSign.NEXT_SIGNIFICANT, new Version(1, 2, 5, null));

        assertFalse("null should not matches ^1.2", c1.matches(null));

        assertTrue("1.5 should matches =1.5", c1.matches(v1_5));
        assertFalse("1.8 should not matches =1.5", c1.matches(v1_8));

        assertTrue("1.8 should matches !1.5", c2.matches(v1_8));
        assertFalse("1.5 should not matches !1.5", c2.matches(v1_5));

        assertTrue("1.30 should matches >1.12", c3.matches(v1_30));
        assertFalse("1.12 should not matches >1.12", c3.matches(v1_12));

        assertTrue("1.12 should matches >=1.12", c4.matches(v1_12));
        assertFalse("1.8 should not matches >=1.12", c4.matches(v1_8));

        assertTrue("1.5 should matches <1.12", c5.matches(v1_5));
        assertFalse("1.12 should not matches <1.12", c5.matches(v1_12));

        assertTrue("1.12 should matches <=1.12", c6.matches(v1_12));
        assertFalse("1.30 should not matches <=1.12", c6.matches(v1_30));

        assertTrue("1.5 should matches ^1.2", c7.matches(v1_5));
        assertFalse("2.5 should not matches ^1.2", c7.matches(v2_5));
        assertTrue("0.2.3 should matches ^0.2", c7_1.matches(v0_2_3));
        assertFalse("0.3 should not matches ^0.2", c7_1.matches(v0_3));

        assertTrue("1.30 should matches ~1.2", c8.matches(v1_30));
        assertFalse("2.5 should not matches ~1.2", c8.matches(v2_5));
        assertTrue("1.2.9 should matches ~1.2.5", c8_1.matches(v1_2_9));
        assertFalse("1.5 should not matches ~1.2.5", c8_1.matches(v1_5));

        assertFalse("0_3 should not matches ~1.2", c8.matches(v0_3));

        // Super weird cases: I think we will never see those
        assertFalse("0.3 should not matches ^0", c7_2.matches(v0_3));
        assertFalse("1 should not matches ^0", c7_2.matches(v1));
        // assertFalse("1 should not matches ~1.2", c8.matches(v1));

        c1.addAnd(c2);
        c1.addOr(c3);
    }

}
