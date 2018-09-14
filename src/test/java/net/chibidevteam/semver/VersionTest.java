package net.chibidevteam.semver;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import lombok.extern.slf4j.Slf4j;
import net.chibidevteam.semver.Version;

@Slf4j
public class VersionTest {

    @Test
    public void ctor() {
        Version v = new Version(1, 2, 3, "x");
        assertTrue("Major should be 1", v.getMajor() == 1);
        assertTrue("Minor should be 2", v.getMinor() == 2);
        assertTrue("Patch should be 3", v.getPatch() == 3);
        assertTrue("PreRelease should be 'x'", "x".equals(v.getPreRelease()));
        assertTrue("Meta should be null", v.getMeta() == null);

        v = new Version(1, 2, 3, "x", new String[] {});
        assertTrue("Major should be 1", v.getMajor() == 1);
        assertTrue("Minor should be 2", v.getMinor() == 2);
        assertTrue("Patch should be 3", v.getPatch() == 3);
        assertTrue("PreRelease should be 'x'", "x".equals(v.getPreRelease()));
        assertTrue("Meta should be null", v.getMeta() == null);

        v = new Version(1, 2, 3, "x", "2017");
        assertTrue("Major should be 1", v.getMajor() == 1);
        assertTrue("Minor should be 2", v.getMinor() == 2);
        assertTrue("Patch should be 3", v.getPatch() == 3);
        assertTrue("PreRelease should be 'x'", "x".equals(v.getPreRelease()));
        assertTrue("Meta should not be empty", !v.getMeta().isEmpty());
        assertTrue("Meta size should be 1", v.getMeta().size() == 1);
        assertTrue("Meta[0] should be '2017'", "2017".equals(v.getMeta().get(0)));
    }

    @Test
    public void compareTo() {
        Version v1 = new Version(1, null, null, null);
        Version v2 = new Version(2, null, null, null);
        assertTrue("v1.compareTo(v2) < 0", v1.compareTo(v2) < 0);

        v1 = new Version(1, 1, null, null);
        v2 = new Version(1, 2, null, null);
        assertTrue("v1.compareTo(v2) < 0", v1.compareTo(v2) < 0);

        v1 = new Version(1, 1, 1, null);
        v2 = new Version(1, 1, 2, null);
        assertTrue("v1.compareTo(v2) < 0", v1.compareTo(v2) < 0);

        v1 = new Version(1, 1, 1, null);
        v2 = new Version(1, 1, 1, null);
        assertTrue("v1.compareTo(v2) == 0", v1.compareTo(v2) == 0);

        v1 = new Version(1, 1, 1, null);
        v2 = new Version(1, 1, 1, "y");
        assertTrue("v1.compareTo(v2) > 0", v1.compareTo(v2) > 0);

        v1 = new Version(1, 1, 1, "x");
        v2 = new Version(1, 1, 1, null);
        assertTrue("v1.compareTo(v2) < 0", v1.compareTo(v2) < 0);

        v1 = new Version(1, 1, 1, "x");
        v2 = new Version(1, 1, 1, "y");
        assertTrue("v1.compareTo(v2) < 0", v1.compareTo(v2) < 0);
    }

    // @Test
    // public void compare() {
    // checkGreaterThan("1.2", "1");
    // checkGreaterThan("1.2.6", "1.2.6-alpha");
    // checkGreaterThan("1.2.6-RC1", "1.2.6-alpha");
    //
    // checkLowerThan("1", "1.2");
    // checkLowerThan("1.0.0-alpha", "1.0.0-beta");
    // checkLowerThan("1.0.0-alpha", "1.0.0-BETA");
    //
    // checkSame("1.2", "1.2");
    // checkSame("1.2+256", "1.2+2017");
    // }
    //
    // private void checkGreaterThan(String v1Str, String v2Str) {
    // try {
    // Version v1 = SemverHelper.deserializeVersion(v1Str);
    // Version v2 = SemverHelper.deserializeVersion(v2Str);
    // assertTrue("Expect '" + v1Str + "' > '" + v2Str + "', should be '" + v1Str + "'.compareTo('" + v2Str + "') > 0", v1.compareTo(v2) > 0);
    // } catch (MalformedVersionException e) {
    // log.error("Cannot create version: Check that VersionHelper is working", e);
    // fail("Cannot create version: Check that VersionHelper is working: " + e.getMessage());
    // }
    // }
    //
    // private void checkLowerThan(String v1Str, String v2Str) {
    // try {
    // Version v1 = SemverHelper.deserializeVersion(v1Str);
    // Version v2 = SemverHelper.deserializeVersion(v2Str);
    // assertTrue("Expect '" + v1Str + "' < '" + v2Str + "', should be '" + v1Str + "'.compareTo('" + v2Str + "') < 0", v1.compareTo(v2) < 0);
    // } catch (MalformedVersionException e) {
    // log.error("Cannot create version: Check that VersionHelper is working", e);
    // fail("Cannot create version: Check that VersionHelper is working");
    // }
    // }
    //
    // private void checkSame(String v1Str, String v2Str) {
    // try {
    // Version v1 = SemverHelper.deserializeVersion("1.2");
    // Version v2 = SemverHelper.deserializeVersion("1.2");
    // assertTrue("Expect '" + v1Str + "' ~= '" + v2Str + "', should be '" + v1Str + "'.compareTo('" + v2Str + "') = 0", v1.compareTo(v2) == 0);
    // } catch (MalformedVersionException e) {
    // log.error("Cannot create version: Check that VersionHelper is working", e);
    // fail("Cannot create version: Check that VersionHelper is working");
    // }
    // }
}
