package net.chibidevteam.semver.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

import net.chibidevteam.semver.Version;
import net.chibidevteam.semver.constraint.Constraint;
import net.chibidevteam.semver.constraint.ConstraintSign;
import net.chibidevteam.semver.exceptions.MalformedVersionException;

public class DeserializerTest {

    @Test
    public void deserializeVersion() {
        try {
            assertEquals(new Version(1, 2, 5, "RC2", "sha256", "2017"),
                    Deserializer.deserializeVersion("1.2.5-RC2+sha256.2017"));
            assertEquals(new Version(1, 2, null, "RC2", "sha256", "2017"),
                    Deserializer.deserializeVersion("1.2-RC2+sha256.2017"));
            assertEquals(new Version(1, null, null, null), Deserializer.deserializeVersion("1"));
            assertEquals(new Version(1, 5, null, null), Deserializer.deserializeVersion("1.5"));
            assertEquals(new Version(1, 5, 9, null), Deserializer.deserializeVersion("1.5.9"));
        } catch (MalformedVersionException e) {
            fail("" + e.getMessage());
        }

        try {
            assertEquals(new Version(null, 2, 5, "alpha"), Deserializer.deserializeVersion(".2.5-alpha"));
            fail("This  should fail  to be parsed");
        } catch (MalformedVersionException e) {
        }
    }

    @Test
    public void getConstraintSign() {
        assertEquals(ConstraintSign.EQ, Deserializer.getConstraintSign("1.2"));
        assertEquals(ConstraintSign.EQ, Deserializer.getConstraintSign("=1.2"));

        assertEquals(ConstraintSign.NOT, Deserializer.getConstraintSign("!1.2"));

        assertEquals(ConstraintSign.GT, Deserializer.getConstraintSign(">1.2"));

        assertEquals(ConstraintSign.GTE, Deserializer.getConstraintSign(">=1.2"));

        assertEquals(ConstraintSign.LT, Deserializer.getConstraintSign("<1.2"));

        assertEquals(ConstraintSign.LTE, Deserializer.getConstraintSign("<=1.2"));

        assertEquals(ConstraintSign.COMPATIBLE, Deserializer.getConstraintSign("^1.2"));

        assertEquals(ConstraintSign.NEXT_SIGNIFICANT, Deserializer.getConstraintSign("~1.2"));
    }

    @Test
    public void getVersion() {
        Version v = new Version(1, 2, null, null);

        try {
            assertEquals(v, Deserializer.getVersion("1.2"));
            assertEquals(v, Deserializer.getVersion("=1.2"));

            assertEquals(v, Deserializer.getVersion("!1.2"));

            assertEquals(v, Deserializer.getVersion(">1.2"));

            assertEquals(v, Deserializer.getVersion(">=1.2"));

            assertEquals(v, Deserializer.getVersion("<1.2"));

            assertEquals(v, Deserializer.getVersion("<=1.2"));

            assertEquals(v, Deserializer.getVersion("^1.2"));

            assertEquals(v, Deserializer.getVersion("~1.2"));
        } catch (MalformedVersionException e) {
            fail("Check Deserializer.getVersion: " + e.getMessage());
        }
    }

    @Test
    public void deserializeConstraint() throws MalformedVersionException {
        Version v = new Version(1, 2, null, null);

        assertEquals(new Constraint(ConstraintSign.EQ, v), Deserializer.deserializeConstraint("1.2"));
        assertEquals(new Constraint(ConstraintSign.EQ, v), Deserializer.deserializeConstraint("=1.2"));

        assertEquals(new Constraint(ConstraintSign.NOT, v), Deserializer.deserializeConstraint("!1.2"));

        assertEquals(new Constraint(ConstraintSign.GT, v), Deserializer.deserializeConstraint(">1.2"));

        assertEquals(new Constraint(ConstraintSign.GTE, v), Deserializer.deserializeConstraint(">=1.2"));

        assertEquals(new Constraint(ConstraintSign.LT, v), Deserializer.deserializeConstraint("<1.2"));

        assertEquals(new Constraint(ConstraintSign.LTE, v), Deserializer.deserializeConstraint("<=1.2"));

        assertEquals(new Constraint(ConstraintSign.COMPATIBLE, v), Deserializer.deserializeConstraint("^1.2"));

        assertEquals(new Constraint(ConstraintSign.NEXT_SIGNIFICANT, v), Deserializer.deserializeConstraint("~1.2"));
    }
}
