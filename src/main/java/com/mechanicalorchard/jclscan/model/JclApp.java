package com.mechanicalorchard.jclscan.model;

import java.util.ArrayList;
import java.util.List;

public class JclApp {
  private List<Job> jobs = new ArrayList<>();
  private Library<Procedure> procLib = new Library<>();
  private Library<Program> linkLib = new Library<>();

  public List<Job> getJobs() {
    return jobs;
  }
  public Library<Procedure> getProcLib() {
    return procLib;
  }
  public Library<Program> getLinkLib() {
    return linkLib;
  }
}
