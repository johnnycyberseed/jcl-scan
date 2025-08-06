package com.mechanicalorchard.jclscan.model;

import java.util.ArrayList;
import java.util.List;

public class JclApp {
  private List<JclFile> jobs = new ArrayList<>();
  private Library<JclProc> procLib = new Library<>();
  private Library<Program> linkLib = new Library<>();

  public List<JclFile> getJobs() {
    return jobs;
  }
  public Library<JclProc> getProcLib() {
    return procLib;
  }
  public Library<Program> getLinkLib() {
    return linkLib;
  }
}
